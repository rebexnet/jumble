namespace Jumble.Analysis

module Stacktrace = 
    open Jumble

    type StacktraceMethodLine = {
        Method: MethodSignature
        Location: string
    }
    
    type StacktraceLine = 
        | StacktraceLine of StacktraceMethodLine
        | OtherLine of string

    module StacktraceLine = 
        let toString stl = 
            match stl with 
            | StacktraceLine sml -> sprintf "STL: at %s%s" (MethodSignature.toString sml.Method) sml.Location
            | OtherLine l -> sprintf "OTL: %s" l
    