namespace Jumble 

[<AutoOpen>]
module rec ElementaryTypes = 
    open System
    open System.Collections
    open System.Reflection
    open Mono.Cecil

    type TypeDefinitionName = {
        /// Name, including namespace
        FullName: string
        GenericParameters: string list
    }
    with
        member this.Name with get() = TypeDefinitionName.splitNamespace this.FullName |> snd
        member this.Namespace with get() = TypeDefinitionName.splitNamespace this.FullName |> fst
    
    module TypeDefinitionName =
        let create name genpars = { FullName = name; GenericParameters = genpars }

        /// ("Foo", "Bar") => "Foo.Bar" when namespace is not null or empty
        let joinNamespaceS ns n = if String.IsNullOrEmpty(ns) then n else $"%s{ns}.%s{n}"
        
        let splitNamespace n =
            match n with 
            | Regex @"(.*(?=\.)|)?\.?(.*)" [namespc; name] ->
                let ns = if String.IsNullOrEmpty(namespc) then None else Some namespc
                (ns, name) 
            | _ -> failwithf $"unable to match %s{n}"
        
        let fromTypeDefinition (td:TypeDefinition) =
            { FullName = match td.Namespace with null | "" -> td.Name | ns -> $"%s{ns}.%s{td.Name}"
              GenericParameters = td.GenericParameters |> Seq.map (fun p -> p.Name) |> Seq.toList }
            
        let fullNameFromTypeReference (tr:TypeReference) =
            joinNamespaceS tr.Namespace tr.Name
        
        let applyTo (tn:TypeDefinitionName) (target:TypeReference) : unit =
            let applyName () =
                let newNamespace, newName = splitNamespace tn.FullName
                
                target.Namespace <- newNamespace |> Option.defaultValue ""
                target.Name <- newName
                
            match target with
            | :? TypeDefinition as target ->
                let originalName = fromTypeDefinition target
                applyName()
                
                if target.GenericParameters.Count <> tn.GenericParameters.Length then
                    failwithf $"Generic parameter count mismatch for type %s{target.FullName}. Expected %i{target.GenericParameters.Count}, got %i{tn.GenericParameters.Length}"
                
                tn.GenericParameters
                |> List.iteri (fun i p -> target.GenericParameters[i].Name <- p)
                
                // Mono.Cecil does not update its cache after type is renamed, apparently.
                // This causes odd behaviour later when exporting the module (type not found, etc.)
                // Still an issue in 0.11.1 
                updateNameCache target originalName tn
            | _ -> applyName()
            
        let private rowCreator =
            let tpModuleDef = typeof<ModuleDefinition>
            let tpRow = tpModuleDef.Assembly.GetType("Mono.Cecil.Metadata.Row`2").MakeGenericType([| typeof<string>; typeof<string> |])
            tpRow.GetConstructor([| typeof<string>; typeof<string> |])
        
        /// Updates name_cache inside ModuleDefinition's TypeDefinitionCollection with new type definition name
        let private updateNameCache (td:TypeDefinition) (orig:TypeDefinitionName) (new':TypeDefinitionName) =
            let fldCache =
                let tpModuleDef = typeof<ModuleDefinition>
                let fldTypes = tpModuleDef.GetField("types", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(td.Module)
                tpModuleDef.Assembly.GetType("Mono.Cecil.TypeDefinitionCollection")
                    .GetField("name_cache", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue(fldTypes) :?> IDictionary
            
            let createRow ns n = 
                rowCreator.Invoke([| ns |> Option.defaultValue ""; n |])
            
            let origNamespace, origName = splitNamespace orig.FullName
            let oldRow = createRow origNamespace origName
            fldCache.Remove(oldRow)
            
            let newNamespace, newName = splitNamespace new'.FullName
            let newRow = createRow newNamespace newName
            if fldCache.Contains(newRow) = false then fldCache.Add(newRow, td)