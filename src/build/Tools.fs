namespace Build

open Fake.Core

module rec ExeHelper =
    let where f =
        execResult @"C:\Windows\System32\where.exe" f
        |> String.split '\n'
        |> List.map String.trim
        |> List.head

    let chkResultFail res =
        if res <> 0 then failwithf "Process exited with non-zero exit code"

    let exec exePath args =
        let result = CreateProcess.fromRawCommandLine exePath args
                     |> Proc.run
        result.ExitCode |> chkResultFail

    let private execResult exePath args =
        let result = CreateProcess.fromRawCommandLine exePath args
                     |> CreateProcess.redirectOutput
                     |> Proc.run

        result.ExitCode |> chkResultFail
        result.Result.Output.Trim()

    let mktool exe arg = exec (where exe) arg
    let mktoolResult exe arg = execResult (where exe) arg