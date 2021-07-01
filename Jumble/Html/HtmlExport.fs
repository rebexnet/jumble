namespace Jumble.Html
open Jumble.Rename.RenameFilter
open Serilog

[<AutoOpen>]
module HtmlExport = 
    open FsHtml
    open Jumble.Analysis
    open System.IO
    
    open Jumble.Html.Helpers

    let grouping (fr: FilterResult) (f:string) =
        let f = Path.GetFullPath(f)
        Log.Information("Exporting rename map to {File:l}", f)
        let groupResult2Html (gr:GroupingResult) = 
            let reasonHtml = 
                match gr.Reason with 
                | NoReason -> %""
                | InterfaceImplementation m -> [
                    Text "implements interface "
                    type2Html Long m.DeclaringType
                  ]
                | OverrideMethod m -> [ 
                    Text "overrides method declared in "
                    type2Html Long m.DeclaringType
                  ]
                | PropertyAccessorMethod p -> % $"belongs to property %s{p.Name}"

            tr [] [ 
                td [] [member2Html gr.Member]
                td [] reasonHtml
            ]

        let group2Html (grs:seq<GroupingResult>) = 
            table [] (grs |> Seq.sortBy(fun r -> r.Member.DeclaringType.FullName) |> Seq.map groupResult2Html |> Seq.toList)
        
        let groups = fr.FilteredGroups |> Array.filter(fun g -> g.Length > 1) |> Array.sortByDescending (fun g -> g.Length)

        let res = html [] [ 
            head [] [ 
                style [] %""" 
BODY { display: flex }
TABLE { border: solid 1px black; margin-bottom: 5px; }
.genparam { color: blue }
.method { font-family: monospace; font-size: smaller }
.property { font-family: monospace; font-size: smaller }
.type { font-family: monospace; color: darkblue }
.type-builtin { color: blue; }
                """
            ]
            body [] [
                div [] (groups |> Seq.map (fun g -> group2Html g) |> Seq.toList)
            ]
        ]

        let resString = toString res
        let dirName = Path.GetDirectoryName(f)
        Directory.CreateDirectory(dirName) |> ignore
        File.WriteAllText(f, resString)

        