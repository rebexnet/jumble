﻿namespace Jumble

open Jumble.Rename
open Mono.Cecil
open Serilog

module TypeRename =
    let private renameType findRefs (t:TypeRenamePlan) : unit =
        // 1. Rename all references
        let (refs:TypeReference array) = findRefs t.Type
        refs |> Array.iter (TypeDefinitionName.applyTo t.NewName)
        
        // 2. Rename type definition
        TypeDefinitionName.applyTo t.NewName t.Type
    
    let renameTypes findRefs (types:TypeRenamePlan[]) : unit =
        Log.Information("Renaming {Types} types...", types.Length)
        types |> Seq.iter (renameType findRefs) 

    /// Splits generic suffix from type name, e.g. Foo`3 => (Foo, `3)
    let private splitGenericSuffix (n:string) =
        match n with
        | Regex @"^(.*)(`\d+)$+" [name; suffix] -> (name, suffix)
        | _ -> (n, "")

    let createRenamePlans (nameGen:NameGenerators.TypeNameGenerator) (genParNameGen:NameGenerators.GenericParameterNameGenerator) (types:TypeTreeNode[]) : TypeRenamePlan[] =
        let createPlan (t:TypeTreeNode) : TypeRenamePlan=
            let tdn = TypeDefinitionName.fromTypeDefinition t.TypeDefinition

            // todo: keeping the suffix is required only for .NET Native UWP compiler - might make this configurable
            let (nameWithoutGenericSuffix, genericSuffix) = splitGenericSuffix tdn.FullName

            let newName = (nameGen nameWithoutGenericSuffix) + genericSuffix

            // remove the namespace for nested types (.NET Native Release builds fail if the namespace is present)
            let newName = if t.TypeDefinition.IsNested then TypeDefinitionName.splitNamespace newName |> snd else newName
            let newTdn = TypeDefinitionName.create newName (List.mapi genParNameGen tdn.GenericParameters)
            printfn $"Renaming %s{t.TypeDefinition.FullName} to {newTdn.FullName}"
            { TypeRenamePlan.Type = t.TypeDefinition; NewName = newTdn; OriginalName = tdn }
            
        types |> Array.map createPlan