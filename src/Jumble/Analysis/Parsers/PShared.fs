namespace Jumble.Analysis.Parsers

module PShared = 
    open FParsec
    open Jumble
    
    type NPT = NamedParameter
    type PT = Parameter
    type SPT = SimpleParameterType
    type WPT = WrappedParameter

    type TypeSuffix =
    | ArraySuffix of ArrayDimension
    | PointerSuffix

    let ws p = spaces >>. p .>> spaces
    let ws_string s = ws (pstring s)

    let private isValidFirstIdentChar (c:char) = c = '_' || isLetter c
    let private isValidOtherIdentChar (c:char) = c = '_' || isLetter c || isDigit c
    
    // Foo
    let pIdentifier : Parser<string, unit> = 
        let j (c:string) (cs:string) = $"%s{c}%s{cs}"
        pipe2 (many1Satisfy isValidFirstIdentChar) (manySatisfy isValidOtherIdentChar) j
    
    // Foo.Bar
    let pIdentifiersSepByDots : Parser<string, unit> = 
        sepBy1 pIdentifier (pchar '.') |>> String.concat "."

    let pword : Parser<string, unit> = 
        many1Satisfy (fun c -> System.Char.IsWhiteSpace(c) = false)

    let pword_ws : Parser<string, unit> = 
        pword .>> spaces

    let unwrap = function Success(result, _, _) -> Result.Ok result | Failure(msg,_,_) -> Result.Error msg
    
    let toRes<'T> (r:Result<'T, _>) = match r with Result.Ok res -> res | Result.Error msg -> failwith msg
    