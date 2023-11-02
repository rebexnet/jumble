namespace Jumble.Analysis

open Jumble
open Mono.Cecil
open Serilog

module NewMethodLookupFunctions =
    let tryFindExplicitMethodImplementationOnDescentant(
        descendant: TypeDefinition,
        parentLink: TypeReference,
        originMethod: MethodDefinition,
        derivedMethod: MethodReference) =

        let isOverrideMatch(m:MethodReference) =
            m.Resolve() = originMethod
            && TypeReference.areEqual m.DeclaringType parentLink
            && MethodReference.compareParameters m derivedMethod

        let isMatch (m:MethodDefinition) = m.Overrides |> Seq.exists isOverrideMatch


        descendant.Methods
        |> Seq.filter isMatch
        |> Seq.trySingle


// todo: rename this, move elsewhere
[<AutoOpen>]
module MethodLookupFunctions =

    // Attempts to find explicit method implementation for 'm' in type 'target' implementing interface through reference 'parentRef'
    // 'parentRef' does not necessarily need to be declared on 'target'
    let tryFindExplicitMethodImplementation (target:TypeDefinition) (parentRef:TypeReference) (m:MethodDefinition) : MethodDefinition option = 
        let isOverride (movr:MethodReference) = 
            TypeReference.areEqual movr.DeclaringType parentRef
            && movr.Resolve() = m

        target.Methods 
        |> Seq.tryFind (fun m -> m.Overrides |> Seq.exists isOverride)

    let private findMethodCandidates (target:TypeDefinition) (md:MethodDefinition) = 
        let findExplicitImplementations () =
            target.Methods |> Seq.filter(fun m -> m.Overrides |> Seq.exists (fun ovr -> ovr.Resolve() = md)) |> Seq.toList
        
        let isPossibleImplImplMethod (m:MethodDefinition) = 
            let res = m.Name = md.Name && m.Parameters.Count = md.Parameters.Count && m.GenericParameters.Count = md.GenericParameters.Count
            res 

        let explImpls = if md.DeclaringType.IsInterface then findExplicitImplementations() else []
        let implImpls = target.Methods |> Seq.filter isPossibleImplImplMethod |> Seq.toList
        List.concat [explImpls; implImpls]
    
    /// Finds method definition that matches 'm' in 'target' or its ancestors
    let rec findInterfaceMethodImplementationInAncestors (target:TypeDefinition) (pathDown:TypeReference list) (m:MethodReference) (md:MethodDefinition) : MethodDefinition =
        assert md.DeclaringType.IsInterface
        assert (not target.IsInterface)
        
        match pathDown with 
        | [] -> 
            // we assume the method was already derived for target
            let candidates = findMethodCandidates target md
            match candidates |> Seq.filter (MethodReference.compareParameters m) |> Seq.tryHead with
            | Some m -> m
            | None -> findInterfaceMethodImplementationInAncestors target [target.BaseType] m md
        | _ ->
            
            if pathDown.Head = null then // no more ancestors to look into
                if md.HasBody && md.IsStatic = false then md else // default interface implementation
                Log.Error(
                    "Cannot find implementation of {InterfaceMethod:l} in type {Type:l} ({Path:l}) and there is nowhere to go up now. We reached this type going up the inheritance chain:",
                    md.FullName, target.FullName, target.Module.FileName)
                pathDown |> List.filter (fun p -> p <> null) |> List.iter (fun p -> Log.Error(" -> {Type:l}", p.FullName))
                failwithf $"Unable to find method %s{m.FullName} in type %s{target.FullName} or its ancestors"
            else 
                let t = TypeReference.safeResolve pathDown.Head
                let candidates = findMethodCandidates t md
                let deriveCandidate (c:MethodReference) : MethodReference = pathDown |> List.fold (fun method path -> Deriver.deriveMethod null path method) c
                let derivedCandidates = candidates |> Seq.map (fun c -> (c, deriveCandidate c))
                match derivedCandidates |> Seq.filter (snd >> MethodReference.compareParameters m) |> Seq.tryHead with
                | Some (m, _) -> m
                | None -> findInterfaceMethodImplementationInAncestors target (t.BaseType::pathDown) m md

    // Finds implementation for 'm' in type 'target' (or ancestors if not found) implementing interface via reference 'parentRef'
    let rec findInterfaceMethodImplementationViaReference (target:TypeDefinition) (parentRef:TypeReference) (m:MethodDefinition) : MethodDefinition =
        assert m.DeclaringType.IsInterface
        
        let derivedMethod = Deriver.deriveMethod m.DeclaringType parentRef m
        findInterfaceMethodImplementationInAncestors target [] derivedMethod m

    /// Finds all implementations for 'm'.
    /// Usually returns only when but for multiple inheritance (with different generic args) it will return more than one. 
    /// 'm' must belong to an interface, 'target' must not be an interface
    let findInterfaceMethodImplementationAcrossMultipleInheritance (target:TypeDefinition) (m:MethodDefinition) : MethodDefinition list =
        assert m.DeclaringType.IsInterface
        assert (not target.IsInterface)
        
        target.Interfaces
        |> Seq.choose(fun i -> if i.InterfaceType.Resolve() = m.DeclaringType then Some i.InterfaceType else None)
        |> Seq.map(fun ref -> findInterfaceMethodImplementationViaReference target ref m)
        |> Seq.toList
    
    let findVirtualMethodOverrides (m:MethodDefinition) (t:TypeTreeNode) : MethodDefinition list =
        let derive (m:MethodReference) (child:TypeTreeNode) = (Deriver.deriveMethod child.TypeDefinition child.TypeDefinition.BaseType m, child)
        let rec fm (m:MethodReference) (t:TypeTreeNode) : MethodDefinition list = 
            let findChildMethods () = 
                t.Children
                |> Seq.map (derive m)
                |> Seq.collect (fun (m, c) -> fm m c)
                |> Seq.toList
            match m.Resolve() with 
            // new declaration
            | null -> findChildMethods()
            | resolved when resolved.DeclaringType = t.TypeDefinition && resolved.IsNewSlot -> []
            | resolved when resolved.DeclaringType = t.TypeDefinition && resolved.IsNewSlot = false -> resolved::findChildMethods()
            | _ -> findChildMethods()

        assert (m.IsNewSlot && m.IsVirtual)
        t.Children |> Seq.toList |> List.map (derive m) |> List.collect(fun (m, c) -> fm m c)