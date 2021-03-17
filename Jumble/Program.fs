open Jumble
open Serilog
open Serilog.Events

[<EntryPoint>]
let main argv =
    Log.Logger <- LoggerConfiguration().MinimumLevel.Is(LogEventLevel.Debug).WriteTo.ColoredConsole().CreateLogger()

    try 
        match argv with
        | [|file|] ->
                let configModel = ConfigurationModel.load file
                let baseDir = System.IO.Path.GetDirectoryName(file)
                let dllOpts = ConfigurationModel.toObfuscationOptions baseDir configModel
                let _ = obfuscate (fun _ -> dllOpts)
                0
        | _ ->
            Log.Error("Usage: dotnet jumble.dll path-to-configuration-file")
            -2

    with e ->
        Log.Fatal(e, "Unhandled exception")
        -1