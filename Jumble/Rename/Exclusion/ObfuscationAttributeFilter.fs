namespace Jumble.Rename.Exclusion

open System.Reflection
open Jumble
open Jumble.Rename
open Mono.Cecil

module ObfuscationAttributeFilter =
    /// Apply exclusion to private members (do not rename private members)
    let [<Literal>] featureApplyToPrivate = "applytoprivate"

    /// Apply exclusion to descendants
    let [<Literal>] featureApplyToChildren = "applytochildren"

    /// Not currently used
    let [<Literal>] private featureAll = "all"

    type private ExcludeScope = AppliesToSelf | AppliesToPublicMembers | AppliesToAllMembers

    type private ExcludeInfo = {
        Scope: ExcludeScope
        ApplyToChildren: bool
    }

    // converts [Obfuscation] to ExcludeInfo
    let private toExcludeInfo (attr:CustomAttribute) =
        let inline attrVal n (defaultValue:^t) =
            attr.Properties
            |> Seq.tryFind(fun p -> p.Name = n)
            |> Option.map(fun p -> p.Argument.Value :?> ^t)
            |> Option.defaultValue defaultValue

        let exclude = attrVal "Exclude" true
        if exclude = false then None else

        let features = attrVal "Feature" featureAll
                       |> (fun s -> s.ToLowerInvariant().Split(';'))
                       |> Seq.map(fun s -> s.Trim())
                       |> Seq.filter(fun s -> s <> "")
                       |> Seq.distinct
                       |> Seq.toList;

        let applyToPublicMembers = attrVal "ApplyToMembers" true
        let applyToPrivateMembers = features |> Seq.contains featureApplyToPrivate

        let scope = match (applyToPublicMembers, applyToPrivateMembers) with
                    | (_, true) -> AppliesToAllMembers
                    | (true, _) -> AppliesToPublicMembers
                    | _ -> AppliesToSelf
        Some { Scope = scope; ApplyToChildren = features |> List.contains featureApplyToChildren }

    // finds [Obfuscation] on given member and returns ExcludeInfo
    let private findAttr (t:IMemberDefinition) =
        t.CustomAttributes
        |> Seq.tryFind (fun a -> a.AttributeType.FullName = typedefof<ObfuscationAttribute>.FullName)
        |> Option.map toExcludeInfo
        |> Option.flatten


    let rec private fltTypeRec (res:TypeNodeResolver) (ei:ExcludeInfo) (stopOnOverride:bool) (t:TypeTreeNode) : ExclusionScopeAndReason seq = seq {
            // child / nested type has its own [Obfuscation] attribute so we stop here - it will be picked up later
            // we do not support any composition of multiple [Obfuscation] attributes here
            if stopOnOverride && Option.isSome (findAttr t.TypeDefinition) then () else

            match ei.Scope with

            | AppliesToSelf ->
                yield ExclusionScopeAndReason.createType t.TypeDefinition AppliesToTypeNameOnly ExclusionReason.Whitelisted
            | AppliesToPublicMembers ->
                yield ExclusionScopeAndReason.createType t.TypeDefinition AppliesToTypeNameOnly ExclusionReason.Whitelisted
                yield! t.Members |> List.filter (fun m -> MemberDefinition.isPublic m) |> List.map (fun m -> ExclusionScopeAndReason.createMember m ExclusionReason.Whitelisted)
            | AppliesToAllMembers ->
                yield ExclusionScopeAndReason.createType t.TypeDefinition MemberInclusion.AppliesToAllMembers ExclusionReason.Whitelisted

            let filterNestedPublicOnly = Seq.filter (fun (t:TypeDefinition) -> t.IsNestedPublic)

            match ei.Scope with
            | AppliesToPublicMembers | AppliesToAllMembers ->
                let innerScope = { Scope = ei.Scope; ApplyToChildren = false }
                yield! t.TypeDefinition.NestedTypes
                       |> (if ei.Scope = AppliesToPublicMembers then filterNestedPublicOnly else (seq<_>))
                       |> Seq.map res
                       |> Seq.collect (fltTypeRec res innerScope true)

            | AppliesToSelf -> ()

            match ei.ApplyToChildren with
            | true -> yield! t.Children |> Seq.collect (fltTypeRec res ei true)
            | false -> ()
        }

    /// Filters types marked with [Obfuscation] attribute
    let fltObfuscationAttributeOnType (res:TypeNodeResolver) (t:TypeFilterContext) =
        match findAttr t.Type.TypeDefinition with
        | None -> Seq.empty
        | Some ei -> fltTypeRec res ei false t.Type