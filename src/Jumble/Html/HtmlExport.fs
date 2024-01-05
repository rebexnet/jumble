namespace Jumble.Html
open Jumble.Rename.RenameFilter
open Serilog

[<AutoOpen>]
module HtmlExport = 
    open Jumble.Analysis
    open System.IO
    
    open Jumble.Html.Helpers

    let grouping (fr: FilterResult) (f:string) =
        let f = Path.GetFullPath(f)
        Log.Information("Exporting rename map to {File:l}", f)
        let groupResult2Html (gr:GroupingResult) = 
            let reasonHtml = 
                match gr.Reason with 
                | NoReason -> ""
                | InterfaceImplementation m -> $"implements interface {type2Html Long m.DeclaringType}"
                | OverrideMethod m -> $"overrides method declared in {type2Html Long m.DeclaringType}"
                | PropertyAccessorMethod p -> $"belongs to property {%p.Name}"

            $"<tr><td>{member2Html gr.Member}</td><td>{reasonHtml}</td></tr>"

        let group2Html (grs:seq<GroupingResult>) =
            let content =
                grs
                |> Seq.sortBy _.Member.DeclaringType.FullName
                |> Seq.map groupResult2Html
                |> String.concat ""

            $"<table>{content}</table>"

        
        let groups = fr.FilteredGroups |> Array.filter(fun g -> g.Length > 1) |> Array.sortByDescending _.Length

        let style = @"
BODY { display: flex }
TABLE { border: solid 1px black; margin-bottom: 5px; }
.genparam { color: blue }
.method { font-family: monospace; font-size: smaller }
.property { font-family: monospace; font-size: smaller }
.type { font-family: monospace; color: darkblue }
.type-builtin { color: blue; }"

        let content = groups |> Seq.map (fun g -> group2Html g) |> String.concat ""

        let res = $"
<html>
    <head>
        <style>{style}</style>
    </head>
    <body>
        <div>{content}</div>
    </body>
</html>"
        let dirName = Path.GetDirectoryName(f)
        Directory.CreateDirectory(dirName) |> ignore
        File.WriteAllText(f, res)