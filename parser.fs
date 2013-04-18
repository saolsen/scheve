module scheve.Parser

//#I "FParsec.0.9.2.0/lib/net40"
//#r "FParsec.dll"

open System
open FParsec

type LispVal =
  | Atom of string
  | List of LispVal list
  | DottedList of LispVal list * LispVal
  | Integer of int64
  | String of string
  | Bool of bool

type Parser<'t> = Parser<'t, unit>

let symbol : Parser<char> =
  anyOf "!#$%&|*+-/:<=>?@^_~"
  
let unescape c =
  match c with
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c   -> c

let normalChar = noneOf "\"\\"

let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

let parseString : Parser<LispVal> =
  between (pstring "\"") (pstring "\"")
          (manyChars (normalChar <|> escapedChar))
  |>> String

let parseAtom : Parser<LispVal> =
  pipe2 (asciiLetter <|> symbol)
        (manyChars (asciiLetter <|> digit <|> symbol))
        (fun first rest -> Atom (first.ToString() + rest))

//TODO: Support all scheme number literals, not just ints
//      complex, real, rational, integer
let parseInteger : Parser<LispVal> =
  pint64 |>> Integer

let parseExpr : Parser<LispVal> =
  parseAtom <|> parseString <|> parseInteger

let parseList : Parser<LispVal> =
  sepBy parseExpr spaces |>> List

//tricky
//let parseDottedList : Parser<LispVal> =
//  pipe2 (parseExpr .>> spaces) (char'.'

let parseQuoted : Parser<LispVal> =
  (pchar '\'' >>. parseExpr) |>> (fun x -> List [(Atom "quote"); x])

// gotta figure out how to add list, dottedlist and quoted to parseExpr when they haven't been defined yet and rely on it....

// for testing
let test p str =
  match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

