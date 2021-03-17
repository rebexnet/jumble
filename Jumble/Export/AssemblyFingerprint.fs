module Jumble.Export.AssemblyFingerprint
open System.IO
open Jumble
open Jumble.Cecil.CSharpExport
open System.Text
open Mono.Cecil

// todo: decide what to do with this, currently is not used
let createFingerprint (a:AssemblyDefinition) =
    let sb = StringBuilder()
    a.MainModule.GetTypes()
    |> Seq.sortBy(fun t -> t.FullName)
    |> Seq.iter (fun t ->
        
        sb.AppendLine(toCSharpSignature t) |> ignore
        TypeDefinition.members t
        |> Seq.map toCSharpSignature
        |> Seq.sort
        |> Seq.iter (fun s -> sb.AppendLine(s) |> ignore)    
    )
    
    sb.ToString()

let saveFingerprint (dir:string) (a:AssemblyDefinition) =
    let fp = createFingerprint a
    let fullPath = Path.Combine(dir, a.Name.Name + ".txt")
    File.WriteAllText(fullPath, fp)