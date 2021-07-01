namespace Jumble

open System.IO
open Jumble
open Jumble.Analysis
open Jumble.Export.Types
open Jumble.Rename
open Jumble.Rename.Exclusion
open Jumble.Rename.NameGenerators
open Jumble.Utils

module rec ConfigurationModel =
    let private toNameGenerator (def: NameGeneratorType) (s: string option) =
        match s with
        | None
        | Some "default" -> def
        | Some "test" -> NameGenTest
        | Some "order" -> NameGenOrder
        | Some "id" -> NameGenIdentity
        | Some "upsideDown" -> NameGenUpsideDown
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
          SigningKey = x.SigningKey |> Option.map (SigningKey.fromSnkFile)
          ExceptFilters = exceptFilters }

    let toObfuscationOptions baseDir (m: ConfigurationModel): ObfuscateParams =
        let exceptFilters = m.AdditionalOptions
                            |> Option.map (fun opts -> seq {
                                if opts.DisableEnumToStringFiltering |> Option.defaultValue false then
                                    yield FltEnumToString
                            })
                            |> Option.map (Seq.toList)
                            |> Option.defaultValue []

        { Dlls = m.Input |> List.map (toDllOptions baseDir exceptFilters)
          Framework = FrameworkVersion.tryParse m.Framework
          Output =
              { ExportFilter = ModifiedOnly
                ExportTarget = FlattenTo(rootPath baseDir m.Output) }
          LogDir = Option.map (rootPath baseDir) m.LogDir
          GenericParameterNameGenerator = toNameGenerator (NameGenOrder) m.GenericParameterNameGenerator
          MethodNameGenerator = toNameGenerator (NameGenDefault Seed.RandomSeed) m.MethodNameGenerator
          ParameterNameGenerator = toNameGenerator (NameGenOrder) m.ParameterNameGenerator
          TypeNameGenerator = toNameGenerator (NameGenDefault Seed.RandomSeed) m.TypeNameGenerator
          SearchPaths = m.SearchPaths |> Option.defaultValue []

          RenameFilters =
              m.Exclude
              |> Option.defaultValue []
              |> List.map Literal
              |> WhitelistFilter.createWhitelistFilters }