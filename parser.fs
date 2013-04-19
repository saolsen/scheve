module Scheve.Parser

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

let parseExpr, parseExprRef = createParserForwardedToRef<LispVal, unit>()

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

//TODO: Full scheme numeric tower
//      complex, real, rational, integer
let parseInteger : Parser<LispVal> =
  pint64 |>> Integer

let parseQuoted : Parser<LispVal> =
  (pchar '\'' >>. parseExpr) |>> (fun x -> List [(Atom "quote"); x])

let parseProperList : Parser<LispVal> =
  many (parseExpr .>> spaces) |>> List

let parseDottedList : Parser<LispVal> =
  pipe2 (many (parseExpr .>> spaces))
        ((pchar '.') >>. spaces >>. parseExpr)
        (fun init last -> DottedList (init, last))

let parseList : Parser<LispVal> =
  between (pchar '(')
          (pchar ')')
          (attempt parseDottedList <|> parseProperList)

do parseExprRef := choice [parseAtom;
                           parseString;
                           parseInteger;
                           parseQuoted;
                           parseList]

// for testing
let test p str =
  match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

