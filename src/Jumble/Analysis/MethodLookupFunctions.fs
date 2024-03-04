namespace Jumble.Analysis

open Jumble
open Mono.Cecil

// todo: rename this, move elsewhere
[<RequireQualifiedAccess>]
module MethodLookupFunctions =
    let findVirtualMethodOverrides (m:MethodDefinition) (t:TypeTreeNode) : MethodDefinition list =
        let derive (m:MethodReference) (child:TypeTreeNode) = (DeriveDown.deriveMethodWithTarget child.TypeDefinition.BaseType m child.TypeDefinition), child
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

        // IsAbstract is needed as F# does not add "newslot" to abstract methods
        assert ((m.IsNewSlot || m.IsAbstract) && m.IsVirtual)
        t.Children |> Seq.toList |> List.map (derive m) |> List.collect(fun (m, c) -> fm m c)