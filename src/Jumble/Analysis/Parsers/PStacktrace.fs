namespace Jumble.Analysis.Parsers

// System.FormatException: Unable to parse the license ---> System.FormatException: License parsing failed. Invalid format.
// at ykq.aav[n](String jz)
// at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)
// --- End of inner exception stack trace ---
// at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)
// at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense)
// at Rebex.SftpServer.Licensing.LicenseData.Load(String[] licenseProbingPaths)
// at yju.Handle(ShowLicenseCliOptions _)
// at System.Dynamic.UpdateDelegates.UpdateAndExecute2[T0,T1,TRet](CallSite site, T0 arg0, T1 arg1)
// at yju.ipk(VerbOptionsBase p)
// at ykb.Main(String[] args)

//System.Exception: uuhhh
//at ConsoleApp1.Program.C`1.Foo[T](Int32 a, String b) in C:\g\ca\ConsoleApp1\ConsoleApp1\Program.cs:line 16
//at ConsoleApp1.Program.Main(String[] args) in C:\g\ca\ConsoleApp1\ConsoleApp1\Program.cs:line 26

open FParsec
open Jumble
open Jumble.Analysis.Parsers.PShared
open Jumble.Analysis.Stacktrace

module PStacktrace =
    type private IdentWithGenBacktick = { Identifier: string; GenCount: int option }

    // `2
    let private pGenBacktick : Parser<int, unit> = 
        pchar '`' >>. pint32

    // Foo`2
    let private pIdentOrCtorWithGenericBacktick : Parser<IdentWithGenBacktick, unit> =   
        let j ident bcktick = { Identifier = ident; GenCount = bcktick }
        pipe2 (pstring ".ctor" <|> pIdentifier) (opt pGenBacktick) j

    let private pTraceNamespaceClassMethod cs : Reply<ClassName * string> = 
        let rec toClassnameMethodName (partsRev:IdentWithGenBacktick list) : ClassName * string = 
            match partsRev with 
            | methodname::clsname::namespaceRev -> 
                let namespc = namespaceRev |> List.rev |> List.map (fun x -> x.Identifier) |> String.concat "."
                let genCount = clsname.GenCount |> Option.defaultValue 0
                let className = ClassName.create (TypeDefinitionName.joinNamespaceS namespc clsname.Identifier) genCount
                (className, methodname.Identifier)
            | _ -> failwithf $"unsupported: %A{partsRev}"

        sepBy1 pIdentOrCtorWithGenericBacktick (pchar '.') |>> List.rev |>> toClassnameMethodName
        <| cs

    // [T]
    let private pMethodGenericParameters : Parser<string list, unit> = 
        pchar '[' >>. (sepBy1 pIdentifier (pchar ',')) .>> pchar ']'

    // ConsoleApp1.Program.C`1.Foo[T](Int32 a, String b)
    let private pMethodSignature : Parser<MethodSignature, unit> = 
        let pPars : Parser<NPT list, unit> = pchar '(' >>. PParameter.pNamedParameters .>> pchar ')'
        let j ((clsname, methodname):ClassName*string) (mgs:string list option) (pars:NPT list) = 
            MethodSignature.create clsname methodname (mgs |> Option.defaultValue []) pars None
            
        pipe3 pTraceNamespaceClassMethod (opt pMethodGenericParameters) pPars j
    
    // at ConsoleApp1.Program.C`1.Foo[T](Int32 a, String b) in C:\g\ca\ConsoleApp1\ConsoleApp1\Program.cs:line 16
    let private pStacktraceMethodLine : Parser<StacktraceLine, unit> = 
        let j met loc = StacktraceLine { Method = met; Location = loc }
        spaces >>. pword_ws >>. pipe2 pMethodSignature (restOfLine false) j

    let private pStacktraceLine cs : Reply<StacktraceLine> = 
        let safeMethodLineParser cs : Reply<StacktraceLine> = 
            try pStacktraceMethodLine <| cs
            with e -> fail e.Message <| cs
           
        (attempt safeMethodLineParser) <|> (restOfLine false |>> OtherLine)
        <| cs

    let private pStacktrace : Parser<StacktraceLine list, unit> = 
        sepBy pStacktraceLine newline

    let parseMethodSignature ms = run pMethodSignature ms |> unwrap
    let parseStacktraceLine l = run pStacktraceLine l |> unwrap
    let parseStacktraceMethodLine l = run pStacktraceMethodLine l |> unwrap
    let parseStacktrace s = run pStacktrace s |> unwrap