namespace Jumble.Console

open System.IO
open FSharp.Json
open Jumble
open Jumble.Rename
open Jumble.Rename.Exclusion
open Jumble.Rename.NameGenerators

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

    let private toNameGenerator (def: NameGeneratorType) (s: string option) =
        match s with
        | None
        | Some "default" -> def
        | Some "test" -> NameGenTest
        | Some "order" -> NameGenOrder
        | Some "id" -> NameGenIdentity
        | Some x -> failwithf $"Name generator %s{x} is not supported"

    let rec private toObfuscationLevel (s: string option) =
        match s with
        | None -> toObfuscationLevel (Some "default")
        | Some x ->
            match x with
            | CI "default"
            | CI "privateOnly" -> PrivateOnly
            | CI "untouchable" -> ObfuscationLevel.Untouchable
            | CI "onlyNecessary"
            | CI "testLib" -> OnlyNecessary
            | CI "privateAndPublic"
            | CI "publicAndPrivate" -> PrivateAndPublic
            | _ -> failwithf $"Obfuscation level %s{x} is not supported"

    let private rootPath root (path: string) =
        match Path.IsPathRooted(path) with
        | true -> path
        | false -> Path.Combine(root, path)

    let private toDllOptions baseDir (exceptFilters: ExclusionFilterName list) (x: ConfigurationInputModel) =
        { DllPath = rootPath baseDir x.File
          ObfuscationLevel = toObfuscationLevel x.ObfuscationLevel
          SigningKey = x.SigningKey |> Option.map SigningKey.fromSnkFile
          ExceptFilters = exceptFilters }

    let toObfuscationOptions baseDir (m: ConfigurationModel): Integration.ObfuscateParams =
        let exceptFilters = m.AdditionalOptions
                            |> Option.map (fun opts -> seq {
                                if opts.DisableEnumToStringFiltering |> Option.defaultValue false then
                                    yield FltEnumToString
                            })
                            |> Option.map Seq.toList
                            |> Option.defaultValue []

        { Dlls = m.Input |> List.map (toDllOptions baseDir exceptFilters)
          Framework = FrameworkVersion.tryParse m.Framework
          OutputDirectory = rootPath baseDir m.Output
          LogDir = Option.map (rootPath baseDir) m.LogDir
          GenericParameterNameGenerator = toNameGenerator NameGenOrder m.GenericParameterNameGenerator
          MethodNameGenerator = toNameGenerator (NameGenDefault Seed.RandomSeed) m.MethodNameGenerator
          ParameterNameGenerator = toNameGenerator NameGenOrder m.ParameterNameGenerator
          TypeNameGenerator = toNameGenerator (NameGenDefault Seed.RandomSeed) m.TypeNameGenerator
          SearchPaths = m.SearchPaths |> Option.defaultValue []

          RenameFilters =
              m.Exclude
              |> Option.defaultValue []
              |> List.map Literal
              |> WhitelistFilter.createWhitelistFilters }