namespace Jumble.Analysis

open System
open System.Collections.Generic
open System.IO
open FSharpPlus
open Jumble
open Jumble.Utils
open Mono.Cecil

[<AutoOpen>]
module Framework =
    type FrameworkFamily =
    | NET
    | NETFramework
    | NETCore
    | NETStandard
    | MonoTouch
    | MonoAndroid
    | XamarinIos
    | XamarinMac

    /// Similar to System.Version but supports prerelease tags separated by hyphen, e.g. 1.2.3-rc1
    [<CustomComparison>]
    [<CustomEquality>]
    type Version =
        {
            Major: int
            Minor: int
            Build: int
            Revision: int
            Prerelease: string option
        }
        with
        member this.IsPrerelease with get() = this.Prerelease.IsNone

        interface IComparable with
            member x.CompareTo y =
                match y with
                | :? Version as y ->
                    let versionCompare = compare (x.Major, x.Minor, x.Build, x.Revision) (y.Major, y.Minor, y.Build, y.Revision)
                    if versionCompare <> 0 then versionCompare else
                    match (x.Prerelease, y.Prerelease) with
                    | None, None -> 0
                    | None, _ -> 1
                    | _, None -> -1
                    | px, py -> compare px py
                | _ -> 1

        override x.Equals(y) =
            match y with
            | :? Version as y -> x.Major = y.Major && x.Minor = y.Minor && x.Build = y.Build && x.Revision = y.Revision && x.Prerelease = y.Prerelease
            | _ -> false

        override this.GetHashCode() =
            HashCode.Combine(this.Major, this.Minor, this.Build, this.Revision, this.Prerelease)

        static member create (major, minor, build, revision) prerelease =
            { Major = major; Minor = minor; Build = build; Revision = revision; Prerelease = prerelease }
        static member createFrom (v:System.Version) prerelease =
            Version.create (v.Major, v.Minor, v.Build, v.Revision) prerelease

        /// Returns true when the numeric fields are equal (disregards prerelease value); otherwise, returns false
        static member isSameNumericVersion (v1:Version) (v2:Version) =
            v1.Major = v2.Major && v1.Minor = v2.Minor && v1.Build = v2.Build && v1.Revision = v2.Revision

        static member tryParse (s:string) =
            let versionString, prerelease = match s.IndexOf('-') with -1 -> (s, None) | i -> (s.Substring(0, i), Some <| s.Substring(i+1))
            let rec tryParseVersion (xs:string list) =
                match xs with
                | [] -> Some []
                | h::tail ->
                    match Int32.TryParse h with
                    | true, i -> tryParseVersion tail |> Option.map (fun rest -> i::rest)
                    | _ -> None

            match tryParseVersion (String.split ["."] versionString |> Seq.toList) with
            | Some [major; minor; build; rev] -> Some <| Version.create (major, minor, build, rev) prerelease
            | Some [major; minor; build] -> Some <| Version.create (major, minor, build, 0) prerelease
            | Some [major; minor] -> Some <| Version.create (major, minor, 0, 0) prerelease
            | Some [major] -> Some <| Version.create (major, 0, 0, 0) prerelease
            | _ -> None

        static member parse (s:string) =
            Version.tryParse s |> Option.defaultWith (fun () -> invalidArg "s" $"Unable to parse version '%s{s}'")

    type FrameworkVersion =
        {
            Family: FrameworkFamily
            Version: Version
        }
        with
        static member create family version =
            { Family = family; Version = version }

        static member createS family (version:string) =
            FrameworkVersion.create family (Version.parse version)

        static member tryParse (s:string) =
            let parseVersion v =
                match v with
                | Regex "^([\d]+).([\d]+)$" [major; minor]
                | Regex "^(\d)(\d)$" [major; minor] ->
                    Version.create (int major, int minor, 0, 0) None
                | Regex "^(\d)$" [major] ->
                    Version.create (int major, 0, 0, 0) None
                | _ -> failwithf $"Cannot parse version %s{v}"

            let fwWithVersion =
                let vrx = @"([\d\.]+)"
                match s with
                | null -> None
                | Regex @"^net([1-4][\d\.]*)$" [v] -> Some (NETFramework, v)
                | Regex @"^net([5-9][\d\.]*)$" [v] -> Some (NET, v)
                | Regex @"^netcoreapp(5[\d\.]*)$" [v] -> Some (NET, v) // we are tolerant
                | Regex (sprintf "^netcoreapp%s$" vrx) [v] -> Some (NETCore, v)
                | Regex (sprintf "^netstandard%s$" vrx) [v] -> Some (NETStandard, v)
                | _ -> None

            fwWithVersion |> Option.map (fun (fw, version) -> FrameworkVersion.create fw (parseVersion version))

        static member parse (s:string) = FrameworkVersion.tryParse s |> Option.get

        static member assemblyDirs (fw:FrameworkVersion) =
            let ifExists dir = if (Directory.Exists(dir)) then Some [dir] else None

            match fw.Family with
            | NET
            | NETCore ->
                // find version matching major.minor but maxing revision
                let dotnetRoot = @"c:\Program Files\dotnet\shared\"
                let baseAssemblyDir = dotnetRoot + "Microsoft.NETCore.App"
                let otherAssemblyDirs = ["Microsoft.AspNetCore.All"; "Microsoft.AspNetCore.App"; "Microsoft.WindowsDesktop.App"]
                                        |> List.map ((+) dotnetRoot)

                let getMatchingDir d =
                    let versions = d
                                   |> Directory.EnumerateDirectories
                                   |> Seq.choose (fun dir -> Path.GetFileName dir |> Version.tryParse |> Option.map (fun ver -> (ver, dir)))
                                   // isSameNumericVersion is required when latest version is required but it's only prerelease
                                   |> Seq.filter (fun (ver, _) -> ver >= fw.Version || Version.isSameNumericVersion ver fw.Version)
                                   |> Seq.toList

                    versions
                    |> Seq.filter (fun (v, _) -> v.Major = fw.Version.Major && v.Minor = fw.Version.Minor)
                    |> Seq.sortByDescending fst
                    |> Seq.tryHead
                    |> Option.orElseWith (fun () -> versions |> Seq.sortBy fst |> Seq.tryHead)
                    |> Option.map snd

                // the NETCore.App directory is required - the rest is optional
                match getMatchingDir baseAssemblyDir with
                | None -> None
                | Some d -> [d] |> List.append (otherAssemblyDirs |> List.choose getMatchingDir) |> Some

            | NETFramework ->
                let versions = @"c:\Windows\Microsoft.NET\Framework64"
                               |> Directory.EnumerateDirectories
                               |> Seq.choose (fun d -> match Version.tryParse ((Path.GetFileName d).Substring(1)) with Some v -> Some (v, [d; Path.Combine(d, "WPF")]) | _ -> None)
                               |> Seq.toList

                match fw.Version.Major with
                // .NET 3.5 actually stores its assemblies in GAC only (c:\Windows\assembly\GAC_MSIL)
                // todo: add support for loading assemblies from GAC (this is long term)
                | v when v >= 2 && v < 5 -> versions |> List.tryFind (fun (v, _) -> v.Major = 4)
                // | v when v >= 2 && v < 4 -> versions |> List.tryFind (fun (v, _) -> v.Major = 2)
                | _ -> None
                |> Option.map snd

            | NETStandard ->
                // NETStandard will fall back to netcore
                let coreVersion = match (fw.Version.Major, fw.Version.Minor) with
                                  | 1, _ -> (1, 0)
                                  | 2, 0 -> (2, 0)
                                  | 2, 1 -> (3, 0)
                                  | major, minor -> failwithf $"NETStandard version %i{major}.%i{minor} is not (yet) supported"
                                  |> fun (major, minor) -> Version.create (major, minor, 0, 0) None
                FrameworkVersion.create NETCore coreVersion
                |> FrameworkVersion.assemblyDirs |> Some

            | MonoAndroid -> ifExists @"c:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\MonoAndroid\v1.0"
            | MonoTouch -> ifExists @"c:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\MonoTouch\v1.0"
            | XamarinIos -> ifExists @"c:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Xamarin.iOS\v1.0"
            | XamarinMac -> ifExists @"c:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Xamarin.Mac\v2.0"

            |> Option.defaultWith(fun () -> failwithf $"Unable to find assembly dlls for %O{fw}")

        override this.ToString() =
            let v = $"%i{this.Version.Major}.%i{this.Version.Minor}"
            match this.Family with
            | NET -> $"net%s{v}"
            | NETFramework -> $"net%s{v}"
            | NETStandard -> $"netstandard%s{v}"
            | NETCore -> $"netcoreapp%s{v}"
            | MonoAndroid -> "monoandroid"
            | MonoTouch -> "monotouch"
            | XamarinIos -> "xamarinios"
            | XamarinMac -> "xamarinmac"

type AssemblyTreeNode (ad:AssemblyDefinition, freferences:unit -> AssemblyTreeNode list) as this =
    let mutable refs : AssemblyTreeNode list = List.empty<AssemblyTreeNode>
    let referencedBy = HashSet<AssemblyTreeNode>()
    let referencedByRec = HashSet<AssemblyTreeNode>()
    let referencesRec = HashSet<AssemblyTreeNode>()
    
    let updateReferenced() = 
        let rec addRefRec (node:AssemblyTreeNode) = 
            if node.ReferencedByRec.Add(this) then
                node.References |> Seq.iter addRefRec    
        
        refs |> Seq.iter (fun r -> 
            r.ReferencedBy.Add(this) |> ignore
            addRefRec r 
        )
        
    let updateReferencesRec() =
        let rec addRefRec (node:AssemblyTreeNode) =
            if referencesRec.Add(node) then node.References |> Seq.iter addRefRec
        
        addRefRec this
    
    member _.Assembly = ad
    member _.FinalizeRefs() =
        refs <- freferences()
        updateReferenced()
        updateReferencesRec()

    member _.ReferencedBy : HashSet<AssemblyTreeNode> = referencedBy
    member _.ReferencedByRec : HashSet<AssemblyTreeNode> = referencedByRec
    member _.References = refs
    member _.ReferencesRec = referencesRec
 
type TypeTreeNode (asmNode:AssemblyTreeNode, t:TypeDefinition, b: TypeTreeNode option, ifaces: TypeTreeNode[]) =
    let ancestors = match b with None -> [] | Some bt -> bt :: bt.Ancestors
    let children = ResizeArray<TypeTreeNode>()
    let descendants = lazy(Seq.append children (children |> Seq.collect (fun t -> t.Descendants)) |> Seq.distinct |> Seq.toList)
    let members = TypeDefinition.members t |> Seq.toList
    let inheritedInterfaces = 
        seq {
            yield! ifaces
            yield! ifaces |> Seq.collect (fun i -> i.Interfaces)
            yield! b |> Option.collect (fun i -> i.Interfaces |> Array.toSeq)
        } |> Seq.distinct |> Seq.toArray

    member _.Ancestors = ancestors
    member _.AssemblyTreeNode = asmNode
    member _.TypeDefinition = t
    member _.Base = b
    member _.Interfaces = ifaces
    member _.Children = children
    member _.Members = members
    member _.InheritedInterfaces = inheritedInterfaces
    
    override x.Equals obj = Object.ReferenceEquals(x, obj)
    override x.GetHashCode () = x.TypeDefinition.GetHashCode()
        
    member this.AncestorsAndSelf = this::this.Ancestors
        
    member _.Descendants with get() = descendants.Value
    member this.DescendantsAndSelf with get() = this::this.Descendants
        
    member _.FindMember name = members |> Seq.filter (fun m -> m.Name = name) |> Seq.exactlyOne

    override this.ToString() = this.TypeDefinition.FullName
    
    interface IComparable with 
        member this.CompareTo obj = 
            match obj with 
            | :? TypeTreeNode as ttn -> 
                if this = ttn then 0 else compare this.TypeDefinition.FullName ttn.TypeDefinition.FullName
            | _ -> -1

    static member fullName (ttn:TypeTreeNode) = ttn.TypeDefinition.FullName

    static member isAttributeType (t:TypeTreeNode) =
        t.AncestorsAndSelf |> List.exists (fun t -> t.TypeDefinition.FullName = "System.Attribute")

// note: calling .Resolve() is SLOW therefore we should NOT be using unless cached
type FieldResolver = FieldReference -> FieldDefinition
type MemberResolver = MemberReference -> IMemberDefinition
type MethodResolver = MethodReference -> MethodDefinition
type TypeResolver = TypeReference -> TypeDefinition

type Resolvers =
    {
        TypeResolver: TypeResolver
        MethodResolver: MethodResolver
        FieldResolver: FieldResolver
    }
    with
        member this.MemberResolver : MemberResolver =
            let f (m:MemberReference) : IMemberDefinition =
                match m with
                | :? TypeReference as tr -> upcast this.TypeResolver tr
                | :? FieldReference as fr -> upcast this.FieldResolver fr
                | :? MethodReference as mr -> upcast this.MethodResolver mr
                | _ -> failwithf $"Member reference type %s{m.GetType().Name} is not supported"
            f
        static member createSafeMemoized() =
            {
                TypeResolver = memoize TypeReference.safeResolve
                MethodResolver = memoize MethodReference.safeResolve
                FieldResolver = memoize FieldReference.safeResolve
            }

type MemberLookup = IMemberDefinition -> MemberReference seq
type TypeLookup = TypeDefinition -> TypeReference array
type MethodLookup = MethodDefinition -> MethodReference array
type FieldLookup = FieldDefinition -> FieldReference array

type Lookups = {
     MemberLookup: MemberLookup
     TypeLookup: TypeLookup
     MethodLookup: MethodLookup
     FieldLookup: FieldLookup
 }