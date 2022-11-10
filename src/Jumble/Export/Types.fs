namespace Jumble.Export

open Jumble.Rename

[<AutoOpen>]
module Types = 
    type ExportedModulePaths = {
        OriginalPath: string
        ExportedPath: string
    }

    type ExportResult = {
        Dlls: ExportedModulePaths list
        ModuleRenamePlans: ModuleRenamePlan[]
    }