module Jumble.Rename.Exclusion.CustomAttributeExtract

/// Find any attribute usages and filter out their type names, their argument types' names and their argument values
// Enum values used as attribute constructor parameters are not properly renamed
// This is probably a Mono.Cecil bug - https://github.com/jbevain/cecil/issues/630

open FSharpPlus
open Jumble
open Mono.Cecil
open Jumble
open Jumble.Analysis
open Jumble.Rename

let rec private extractExclusionFromAttrArg (arg: CustomAttributeArgument) =
    [
        // typeof(xxx)
        if arg.Type.FullName = "System.Type" then
            match arg.Value with
            | null -> ()
            | :? TypeReference as tr -> yield ExclusionScopeAndReason.createType (TypeReference.safeResolve tr) AppliesToTypeNameOnly CustomAttributeValue
            | _ -> invalidOp "Unexpected type definition"
        else
            let argTypeDef = TypeReference.safeResolve arg.Type
            // todo: ignore known types
            if argTypeDef.IsEnum then
                // todo: we can ignore enum fields which are actually not present in attribute arg
                yield ExclusionScopeAndReason.createType argTypeDef AppliesToAllMembers CustomAttributeValue
            else
                yield ExclusionScopeAndReason.createType argTypeDef AppliesToTypeNameOnly CustomAttributeValue

                match arg.Value with
                | null -> ()
                | :? (CustomAttributeArgument[]) as attrs -> yield! attrs |> Seq.collect extractExclusionFromAttrArg |> Seq.toList
                | :? CustomAttributeArgument as attr -> yield! extractExclusionFromAttrArg attr
                | x ->
                    let t = x.GetType()
                    if (t.Namespace <> "System") then failwithf "Unexpected primitive type %s" (t.FullName)
    ]

// Attributes can be used using public properties, such as [Foo(Index = 5)]
let rec private extractAttrProperties (a:TypeDefinition) =
    [
        yield! a.Properties
               |> Seq.map(fun prop -> prop.SetMethod)
               |> Seq.filter (fun m -> m <> null && m.IsPublic)
               |> Seq.map(fun m -> ExclusionScopeAndReason.createMember m CustomAttributeValue)

        match a.BaseType with
        | null -> ()
        | bt -> yield! extractAttrProperties (TypeReference.safeResolve bt)
    ]

let private extractExclusionsFromAttr (a:CustomAttribute) =
    [
        let attrType = TypeReference.safeResolve a.AttributeType
        yield ExclusionScopeAndReason.createType attrType AppliesToTypeNameOnly CustomAttributeValue
        yield! extractAttrProperties attrType
        yield! a.ConstructorArguments |> Seq.collect extractExclusionFromAttrArg
    ]

let fltCustomAttributeCtorVals (resolve:_) (a:ICustomAttributeProvider) : ExclusionScopeAndReason list =
    a.CustomAttributes |> Seq.toList |> List.collect extractExclusionsFromAttr