namespace Jumble.Rename

open Jumble
open Jumble.Cecil
open Mono.Cecil
open System
open System.Collections.Generic

module NameGenerators =
    let testNameSuffix = "_JUMBLE"
    
    type Seed = 
    | RandomSeed 
    | Seed of int

    type NameGeneratorType = 
    | NameGenIdentity
    | NameGenTest
    | NameGenUpsideDown
    | NameGenDefault of Seed
    | NameGenOrder
    
    type MethodNameGenerator = IMemberDefinition[] -> string
    type ParameterNameGenerator = ParameterDefinition -> string
    type GenericParameterNameGenerator = int -> string -> string
    type TypeNameGenerator = string -> string

    let private defaultIdentifierLength = 5

    let private getAllClassNames (a:AssemblyDefinition) = 
        AssemblyDefinition.allTypes a |> Seq.map (fun t -> t.Name)

    let private getAllMethodNames (a:AssemblyDefinition) = 
        AssemblyDefinition.allTypes a |> Seq.collect (fun t -> t.Methods) |> Seq.map MemberDefinition.canonicalName
    
    let private buildRng seed = 
        match seed with RandomSeed -> Random() | Seed seed -> Random(seed)

    let rec private namegen (rng:Random) (names:HashSet<string>) length = 
        let name = String.Create(length, null, fun span _ -> for i = 0 to span.Length - 1 do span.[i] <- char <| rng.Next(97, 123))
        if (names.Contains name) = false then 
            names.Add(name) |> ignore
            name
        else 
            namegen rng names length

    let buildDefaultTypeGen seed (assemblies:AssemblyDefinition seq) : TypeNameGenerator = 
        let names = HashSet<string>(assemblies |> Seq.collect getAllClassNames)
        let rng = buildRng seed
        let namespc = namegen rng names defaultIdentifierLength
        fun _tdn -> TypeDefinitionName.joinNamespaceS namespc (namegen rng names defaultIdentifierLength) 
       
    let buildDefaultMethodGen seed (assemblies:AssemblyDefinition seq) : MethodNameGenerator = 
        let names = HashSet<string>(assemblies |> Seq.collect getAllMethodNames)
        let rng = buildRng seed
        
        fun _ -> namegen rng names defaultIdentifierLength
    
    let private reverse s = s |> Seq.rev |> Seq.toArray |> System.String
    let private upsideDownUppercaseChars = "Z⅄XMΛ∩┴SɹQԀONW˥ʞſIHפℲƎpƆq∀" |> reverse
    let private upsideDownLowercaseChars = "zʎxʍʌnʇsɹbdouɯlʞɾᴉɥƃɟǝpɔqɐ" |> reverse
    
    let orderGenericParameterGen : GenericParameterNameGenerator =
        fun index _name -> sprintf "T%i" index
        
    let orderParameterGen : ParameterNameGenerator =
        fun p -> sprintf "p%i" p.Index
    
    let upsideDownChar c = 
        if c >= 'a' && c <= 'z' then upsideDownLowercaseChars.[int c - int 'a']
        elif c >= 'A' && c <= 'Z' then upsideDownUppercaseChars.[int c - int 'A']
        else c
    
    let upsideDown (s:string) = 
        (reverse s).ToCharArray() |> Array.map upsideDownChar |> (fun cs -> System.String(cs))
        
    let upsideDownMethodGen : MethodNameGenerator = 
        fun members -> members |> Array.head |> MemberDefinition.canonicalName |> upsideDown

    let upsideDownTypeGen : TypeNameGenerator = 
        upsideDown

    let testingMethodGenF name =
        name + testNameSuffix

    let testingMethodGen : MethodNameGenerator = 
        fun members -> members |> Array.head |> MemberDefinition.canonicalName |> testingMethodGenF
    
    let testingTypeGen : TypeNameGenerator = 
        fun tn -> tn + testNameSuffix 

    let identityGenericParameterGen : GenericParameterNameGenerator =
        fun _index -> id
        
    let identityTypeGen : TypeNameGenerator = id
    
    let identityParameterGen : ParameterNameGenerator =
        fun p -> p.Name

    let identityMethodGen: MethodNameGenerator = 
        fun members -> members |> Array.head |> MemberDefinition.canonicalName

    let buildMethodGen typeName (assemblies:AssemblyDefinition seq) = 
        match typeName with 
        | NameGenDefault seed -> buildDefaultMethodGen seed assemblies
        | NameGenIdentity -> identityMethodGen
        | NameGenTest -> testingMethodGen
        | NameGenUpsideDown -> upsideDownMethodGen
        | _ -> raise (NotSupportedException(sprintf "Method name generator %A is not supported" typeName))
        
    let buildParameterGen typeName =
        match typeName with
        | NameGenIdentity -> identityParameterGen
        | NameGenOrder -> orderParameterGen
        | _ -> raise (NotSupportedException(sprintf "Parameter name generator %A is not supported" typeName))

    let buildGenenericParameterGen typeName =
        match typeName with
        | NameGenIdentity -> identityGenericParameterGen
        | NameGenOrder -> orderGenericParameterGen
        | _ -> raise (NotSupportedException(sprintf "Generic parameter name generator %A is not supported" typeName))
        
    let buildTypeGen typeName (assemblies:AssemblyDefinition seq) = 
        match typeName with 
        | NameGenDefault seed -> buildDefaultTypeGen seed assemblies
        | NameGenIdentity -> identityTypeGen
        | NameGenTest -> testingTypeGen
        | NameGenUpsideDown -> upsideDownTypeGen
        | NameGenOrder -> raise (NotSupportedException(sprintf "Type name generator %A is not supported" typeName))