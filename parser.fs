module Scheve.Parser

//#I "FParsec.0.9.2.0/lib/net40"
//#r "FParsec.dll"

open System
open FParsec

type LispVal =
    | Atom of string
    | Integer of int
    | String of string
    | Bool of bool
    | List of LispVal list
    | DottedList of LispVal list * LispVal

type LispError =
  | NumArgs of int * LispVal list
  | TypeMismatch of string * LispVal
//  | Parser /put in whatever the parser error type is
  | BadSpecialForm of string * LispVal
  | NotFunction of string * string
  | UnboundVar of string * string
  | Default of string

type MaybeLispVal =
  | LispVal of LispVal
  | LispError of LispError

exception NotALispVal
exception NotALispError
exception UncheckedType

let isLispVal x =
  match x with
    | LispVal _ -> true
    | _         -> false

let isLispError x =
  match x with
    | LispError _ -> true
    | _           -> false

let getLispVal x =
  match x with
    | LispVal x -> x
    | _         -> raise NotALispVal

let getLispError x =
  match x with
    | LispError x -> x
    | _           -> raise NotALispError

// Parsing
type Parser<'t> = Parser<'t, unit>
  
let unescape c =
  match c with
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c   -> c

let parseExpr, parseExprRef = createParserForwardedToRef<LispVal, unit>()

let symbol = anyOf "!#$%&|*+-/:<=>?@^_~"

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

let parseBool : Parser<LispVal> =
  (pstring "#f" |>> (fun x -> Bool false)) <|>
  (pstring "#t" |>> (fun x -> Bool true))  

//TODO: Full scheme numeric tower
//      complex, real, rational, integer
let parseInteger : Parser<LispVal> =
  pint64 |>> (int >> Integer)

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

do parseExprRef := choice [parseBool;
                           parseAtom;
                           parseString;
                           parseInteger;
                           parseQuoted;
                           parseList]

// Printing
let rec showVal value =
  let showList l =
    match l with
      | []  -> ""
      | [x] -> showVal x
      | x :: xs -> List.fold (fun s v -> s + " " + showVal v) (showVal x) xs
  
  match value with
    | Atom name -> name
    | Integer i -> i.ToString()
    | String s -> "\"" + s + "\""
    | Bool true -> "#t"
    | Bool false -> "#f"
    | List l -> "(" + showList l + ")"
    | DottedList (init,last) -> "(" + showList init + " . " + showVal last + ")"

//TODO
let showErr error = "error"

// Primitives

// treats all non integers as 0, should error instead
let unpackInteger i =
  match i with
    | Integer i -> i
    | _         -> raise UncheckedType

let isInteger i =
  match i with
    | Integer i -> true
    | _         -> false

let numericOperator fn args =
  match List.tryFind (fun x -> not (isInteger x)) args with
    | Some nonInt -> LispError (TypeMismatch ("wanted int, got", nonInt))
    | None ->
      List.reduce fn (List.map unpackInteger args) |> Integer |> LispVal

let primitives = Map [("+", numericOperator (+));
                      ("-", numericOperator (-));
                      ("*", numericOperator (*));
                      ("/", numericOperator (/));
                      ("mod", numericOperator (%));
                      //("quotient", numericOperator quot);
                      //("remainder", numericOperator rem);
                       ]

// Evaluation
let apply f args =
  match Map.tryFind f primitives with
    | Some fn -> (fn args)
    | None    -> LispError (NotFunction ("function doesn't exist", f))

let rec eval lisp =
  match lisp with
    //need the rest of these or going to get errors.
    | String _ as value -> LispVal value
    | Integer _ as value -> LispVal value
    | Bool _ as value -> LispVal value
    | List [Atom "quote"; value] -> LispVal value
    | List (Atom func :: args) ->
      let results = (List.map eval args) in
        if List.forall isLispVal results then
          apply func (List.map getLispVal results)
        else
          (List.find isLispError results)

// for testing
let test p str =
  match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseViewAst str = test parseExpr str
    
let parseAndShow str =
  match run parseExpr str with
    | Success(result, _, _)   -> printfn "Success: %s" (showVal result)
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let repl str =
  match run parseExpr str with
    | Failure(errorMsg,_,_) -> printfn "Failure: %s" errorMsg
    | Success(result,_,_)   ->
      match (eval result) with
        | LispVal v   -> printfn "%s" (showVal v)
        | LispError e -> printfn "%s" (showErr e)

