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
            | StacktraceLine sml -> $"STL: at %s{MethodSignature.toString sml.Method}%s{sml.Location}"
            | OtherLine l -> $"OTL: %s{l}"
    