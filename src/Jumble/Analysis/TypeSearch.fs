namespace Jumble.Analysis

open System.Reflection

open FSharp.Core.Fluent
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Mono.Cecil

open Jumble

module TypeSearch =
    let rec private deriveLambda expr =
        let recursive = deriveLambda
        match expr with
        | Lambda (_, body) -> recursive body
        | Let (_, _, expr2) -> recursive expr2
        | Call (_, methodInfo, _) -> methodInfo
        | _ -> failwithf $"%A{expr} is not supported"

    /// get MethodInfo using an expression
    let method<'T, 'U> (expr:Expr<'T -> 'U>) : MethodInfo =
        deriveLambda expr

    let findTypeNode<'T> (ttns:TypeTreeNode seq) : TypeTreeNode = 
        let t = typedefof<'T>
        let asmName = t.Assembly.GetName().Name
        ttns.find(fun ttn -> ttn.Name.Name = t.Name && ttn.Assembly.Assembly.Name.Name = asmName)
    
    let findMethod<'T, 'U> (ttn:TypeTreeNode) (expr:Expr<'T -> 'U>) : MethodDefinition =
        let mi = method<'T, 'U> expr
        let method = ttn.MemberDefinitions
                     |> Seq.choose (function :? MethodDefinition as m -> Some m | _ -> None)
                     |> Seq.filter (fun m -> m.Name = mi.Name && m.Parameters.Count = mi.GetParameters().Length)
                     |> Seq.exactlyOne
        method
    
    let findTypeMethod<'T, 'U> (ttns:TypeTreeNode seq) (expr:Expr<'T -> 'U>) : TypeTreeNode * MethodDefinition =
        let ttn = findTypeNode<'T> ttns
        (ttn, findMethod<'T, 'U> ttn expr)