namespace Jumble.Analysis.Parsers

open FParsec

open Jumble
open Jumble.Analysis.Parsers.PShared

module rec PParameter = 
    // Foo.Bar
    let private pSimpleParameter cs : Reply<SPT> = 
        pIdentifiersSepByDots |>> SimpleType <| cs

    // [,,,]
    let private pArraySuffix : Parser<TypeSuffix, unit> = 
        let count commas = (List.length commas) + 1
        pstring "[" >>. many (pstring ",") .>> pstring "]" |>> count |>> ArraySuffix

    // ***
    let private pPointerSuffix : Parser<TypeSuffix, unit> = 
        pstring "*" >>. preturn PointerSuffix

    // [] | *
    let private pSuffixes : Parser<TypeSuffix list, unit> = 
        many (pArraySuffix <|> pPointerSuffix)

    // <...>
    let private pGenericArgs cs : Reply<PT list> = 
        let createGenericParam n : PT list= 
            if n = 1 then [Parameter.toGenericType "T"] else 
            [ for i = 1 to n do yield Parameter.toGenericType $"T%i{i}"]

        (pchar '<' >>. pParameters .>> pchar '>') 
        <|>  
        (pchar '`' >>. pint32 |>> createGenericParam)
        <| cs
    
    // out | ref
    let private pByRefPrefix : Parser<string, unit> = 
        (pstring "out" <|> pstring "ref") .>> spaces1

    // Foo | Bar<...>
    let private pSimpleOrGeneric : Parser<PT, unit> = 
        let p (pt:SPT) (args:PT list option) = 
            match args with 
            | Some args -> GenericParameter (pt, args)
            | None -> SimpleParameter pt

        pipe2 pSimpleParameter (opt pGenericArgs) p

    // Foo<...>[]*
    let private pNonByRefParameter : Parser<PT, unit> = 
        let rec p (pt:PT) (suffixes:TypeSuffix list) = 
            match suffixes with 
            | [] -> pt
            | ArraySuffix dim::t -> p (WrappedParameter <| ArrayParameter (pt, dim)) t
            | PointerSuffix::t -> p (WrappedParameter <| PointerParameter pt) t

        pipe2 pSimpleOrGeneric pSuffixes p
    
    // ref Foo<...>[]*
    let pParameter : Parser<PT, unit> = 
        let p (byref:string option) (pt:PT) = match byref with None -> pt | _ -> WrappedParameter <| ByRefParameter pt
        pipe2 (opt pByRefPrefix) pNonByRefParameter p

    let pParameters : Parser<PT list, unit> = 
        let parameterSeparator = ws_string ","
        sepBy pParameter parameterSeparator
    
    let pNamedParameter : Parser<NPT, unit> = 
        let j p (n: string option) = { NamedParameter.Name = n; Type = p }
        pipe2 pParameter (opt (spaces1 >>. pIdentifier)) j

    let pNamedParameters : Parser<NPT list, unit> = 
        let parameterSeparator = ws_string ","
        sepBy pNamedParameter parameterSeparator
    
    let parseNamedParameters = run PParameter.pNamedParameters >> unwrap
    let parseParameter = run PParameter.pParameter >> unwrap
    let parseNamedParameter = run PParameter.pNamedParameter >> unwrap