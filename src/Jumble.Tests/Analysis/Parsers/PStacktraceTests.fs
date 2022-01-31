module PStacktraceTests

open NUnit.Framework
open Jumble
open Jumble.Analysis.Stacktrace
open Jumble.Analysis.Parsers.PStacktrace
open Jumble.Analysis.Parsers.PShared

let sampleStacktrace = """  System.FormatException: Unable to parse the license ---> System.FormatException: License parsing failed. Invalid format.
at ykq.aav[n](String jz)
at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)
--- End of inner exception stack trace ---
at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)
at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense)
at Rebex.SftpServer.Licensing.LicenseData.Load(String[] licenseProbingPaths)
at yju.Handle(ShowLicenseCliOptions _)
at System.Dynamic.UpdateDelegates.UpdateAndExecute2[T0,T1,TRet](CallSite site, T0 arg0, T1 arg1)
at yju.ipk(VerbOptionsBase p)
at ykb.Main(String[] args)"""

let sampleStacktraceLines = """at ykq.aav[n](String jz)
at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)
at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)
at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense)
at Rebex.SftpServer.Licensing.LicenseData.Load(String[] licenseProbingPaths)
at yju.Handle(ShowLicenseCliOptions _)
at System.Dynamic.UpdateDelegates.UpdateAndExecute2[T0,T1,TRet](CallSite site, T0 arg0, T1 arg1)
at yju.ipk(VerbOptionsBase p)
at ykb.Main(String[] args)
at System.Exception.add_SerializeObjectState(EventHandler`1 value)
at Rebex.SafeSerializer_TEST..ctor(ISafeSerializable_TEST obj)
at Rebex.Net.Proxy_TEST..ctor(ProxyType proxyType, ProxyAuthentication authenticationMethod, String host, Int32 port, Boolean bypassOnLocal, NetworkCredential credentials)
at Rebex.Net.Proxy_TEST..ctor(ProxyType proxyType, String host, Int32 port)
at TestProgram.Program.Main() in C:\g\obf\TestProjects\TestProgram\Program.cs:line 13
"""



let private compare<'T> (parser:string->Result<'T, _>) pString (expected:'T) = 
    Assert.AreEqual(expected, parser pString |> toRes<'T>)
   
let private makeTestMethodSignature named = 
    let pars = [Parameter.toSimple "Int32"; Parameter.toSimple "String"] 
               |> if named then List.map2 NamedParameter.create ["a"; "b"] else List.map NamedParameter.createUnnamed 

    MethodSignature.create (ClassName.fromFullname "ConsoleApp.Program.C`1") "Foo" ["T"] pars None

[<Test>]
let MethodSignature () = 
    compare parseMethodSignature "ConsoleApp.Program.C`1.Foo[T](Int32 a, String b)" (makeTestMethodSignature true)

[<Test>]
let ``MethodSignature - no param names`` () = 
    compare parseMethodSignature "ConsoleApp.Program.C`1.Foo[T](Int32, String)" (makeTestMethodSignature false)

[<Test>]
let ``Stacktrace line - method line`` () = 
    let exp1 = StacktraceLine { Method = (makeTestMethodSignature true); Location = " in C:\g\ca\ConsoleApp1\ConsoleApp1\Program.cs:line 16"} 
    compare parseStacktraceLine "at ConsoleApp.Program.C`1.Foo[T](Int32 a, String b) in C:\g\ca\ConsoleApp1\ConsoleApp1\Program.cs:line 16" exp1
    
    let pars2 = [Parameter.toSimple "String" |> NamedParameter.create "serializedLicense"
                 Parameter.toArray 1 (Parameter.toSimple "Byte") |> NamedParameter.create "key"]
    let method2 = Jumble.MethodSignature.create (ClassName.fromFullname "Rebex.SftpServer.Licensing.LicenseData") "Parse" [] pars2 None

    let exp2 = StacktraceLine { Method = method2; Location = "" }
    compare parseStacktraceLine "at Rebex.SftpServer.Licensing.LicenseData.Parse(String serializedLicense, Byte[] key)" exp2


[<Test>]
let ``Stacktrace line - method (constructor) line`` () = 
    let method = Jumble.MethodSignature.create(ClassName.fromFullname "NS.CLS") ".ctor" [] [] None
    let exp = StacktraceLine { Method = method; Location = "" }
    compare parseStacktraceLine "at NS.CLS..ctor()" exp

[<Test>]
let ``Stacktrace line - other line`` () = 
    let exp = OtherLine "System.Exception: uuhhh"
    compare parseStacktraceLine "System.Exception: uuhhh" exp

[<Test>]
let ``Stacktrace parsing does not fail`` () = 
    let parsed = parseStacktrace sampleStacktrace |> toRes
    printfn $":::%A{parsed}:::"
    Assert.AreEqual(11, parsed.Length)

[<Test>]
let ``Stacktrace line results in StacktraceLine`` () = 
    let lines = sampleStacktraceLines.Split([|'\r'; '\n'|], System.StringSplitOptions.RemoveEmptyEntries)
    for line in lines do 
        match parseStacktraceMethodLine line with 
        | Error msg -> Assert.Fail $"Failed on '%s{line}' with msg %s{msg}"
        | Ok line -> printfn $"%A{line}"