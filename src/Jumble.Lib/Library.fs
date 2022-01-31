namespace Jumble

open System.Diagnostics
open FSharp.Json
open System.IO

type ConfigurationModelAdditionalOptions = {
    DisableEnumToStringFiltering: bool option
}

type ConfigurationModel = {
    Framework: string
    Input: ConfigurationInputModel list
    Output: string
    LogDir: string option
    GenericParameterNameGenerator: string option
    MethodNameGenerator: string option
    ParameterNameGenerator: string option
    SearchPaths: string list option
    TypeNameGenerator: string option
    Version: float option
    Exclude: string list option
    AdditionalOptions: ConfigurationModelAdditionalOptions option
}
and ConfigurationInputModel = {
    File: string
    ObfuscationLevel: string option
    SigningKey: string option
}

module rec ConfigurationModel =
    let load s =
        File.ReadAllText s |> parse
        
    let parse s =
        Json.deserialize<ConfigurationModel> s
    let serialize (m:ConfigurationModel) =
        Json.serialize m
        
    let save (path:string) (m:ConfigurationModel) =
        let serialized = serialize m
        File.WriteAllText(path, serialized)
        
    let defaultConfig = {
        Framework = "netstandard2.0"
        Input = []
        Output = "."
        LogDir = None
        GenericParameterNameGenerator = None
        MethodNameGenerator = None
        ParameterNameGenerator = None
        SearchPaths = None
        TypeNameGenerator = None
        Version = None
        Exclude = None
        AdditionalOptions = None
    }

module JumbleApp =
    let obfuscate (appPath: string) (c: ConfigurationModel -> ConfigurationModel) =
        let tempFile = Path.GetTempFileName()
        try
            c ConfigurationModel.defaultConfig |> ConfigurationModel.save tempFile
            let psi = ProcessStartInfo(appPath, $"\"%s{tempFile}\"")
            let ps = Process.Start(psi)
            ps.WaitForExit()
            ps.ExitCode
        finally
            File.Delete(tempFile)    