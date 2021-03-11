﻿namespace Jumble.Utils

open System.Collections.Generic
open System.IO

[<AutoOpen>]
module RegexPatterns =
    open System.Text.RegularExpressions
    
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (|RegexC|_|) (regex:Regex) input = 
        match regex.Match(input) with 
        | m when m.Success -> Some(List.tail [ for g in m.Groups -> g.Value ])
        | _ -> None

    let comparePaths p1 p2 = Path.GetFullPath(p1) = Path.GetFullPath(p2)
        

module Seq =
    let mapArray f xs = xs |> Seq.map f |> Seq.toArray
    let mapList f xs = xs |> Seq.map f |> Seq.toList

    /// Combines partition and map together
    let rec partitionMap f (xs:'T list) = 
        match xs with 
        | [] -> [], []
        | h::tail -> 
            let (r1, r2) = partitionMap f tail
            match f h with 
            | Choice1Of2 c -> c::r1, r2
            | Choice2Of2 c -> r1, c::r2

module Option = 
    let collect (f:'T -> seq<'U>) (x:'T option) = 
        match x with
        | None -> Seq.empty
        | Some v -> f v
        
    let fromBool (x:'T) (b:bool) =
        if b then Some x else None

    let toSeq (x:'T option) =
        match x with Some v -> Seq.singleton v | None -> Seq.empty
        
[<AutoOpen>]        
module Utils = 
    type CustomComparer<'T> (eq, ghc) = 
        interface IEqualityComparer<'T> with 
            member __.Equals(x,y) = eq x y
            member __.GetHashCode(x) = ghc x

    type IdentifierSpec =
    /// Matches the identifier exactly. Use Foo`1 to denote a generic type
    | Literal of string
    
    /// Performs a case-insensitive match
    let (|CI|_|) (str:string) arg = 
        if System.String.Compare(str, arg, System.StringComparison.OrdinalIgnoreCase) = 0
            then Some() else None

    module IdentifierSpec =
        let matches identSpec s =
            match identSpec with
            | Literal l -> s = l
    
    let buildComparer<'T> eq ghc = CustomComparer<'T>(eq, ghc)

    let buildCacheWithComparer (f: 'T -> 'U) (comparer:IEqualityComparer<'T>) = 
        let dict = Dictionary<'T, 'U>(comparer)
        let res key = 
            match dict.TryGetValue(key) with 
            | true, value -> value
            | _ ->
                let value = f key
                dict.Add(key, value)
                value
        res

    let buildCache (f: 'T -> 'U when 'T : equality) = 
        buildCacheWithComparer f EqualityComparer<'T>.Default