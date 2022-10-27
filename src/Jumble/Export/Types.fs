namespace Jumble.Export

open FSharpPlus

[<AutoOpen>]
module Types = 
    open Mono.Cecil
    open Jumble
    open Jumble.Rename

    type ExportFilterStrategy = 
    /// Copies only modified dlls
    | ModifiedOnly

    type ExportTargetStrategy = 
    /// Do not save anything
    | DryRun
    /// Disregards original directory structure and puts all files in same directory
    | FlattenTo of string
    
    type OutputOptions = {
        ExportFilter: ExportFilterStrategy
        ExportTarget: ExportTargetStrategy
    }

    type ExportedModulePaths = {
        OriginalPath: string
        ExportedPath: string
    }

    type ExportResult = {
        Dlls: ExportedModulePaths list
    }