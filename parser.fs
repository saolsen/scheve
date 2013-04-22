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
  | TypeMismatch of string * LispVal
  | ParseError of string
  | NotFunction of string
  | NotImplemented of string
  | NumArgs of int * int
  //| BadSpecialForm of string * LispVal
  //| UnboundVar of string * string
  //| Default of string

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

let showErr error =
  "Error: " +
  match error with
    | TypeMismatch (expected, value) -> "Type Mismatch\n" +
                                        "Expected: " + expected + "\n" +
                                        "Got: " + (showVal value)
    | ParseError error -> "ParseError\n" + error
    | NotFunction f -> "Value is not a function: " + f
    | NotImplemented f -> "Function is not implemented: " + f
    | NumArgs (needed, got) -> "Wrong number of arguments\n" +
                               "Needed: " + needed.ToString() + " Got: " + got.ToString()

// Primitives
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
    | Some nonInt -> LispError (TypeMismatch ("Integer", nonInt))
    | None ->
      List.reduce fn (List.map unpackInteger args) |> Integer |> LispVal

let isBool b =
  match b with
    | Bool b -> true
    | _      -> false

let numBoolBinOp fn args =
  match args with
    | [Integer a; Integer b] -> (fn a b) |> Bool |> LispVal
    | [otherA; Integer _] -> LispError (TypeMismatch ("Integer", otherA))
    | [_; otherB]         -> LispError (TypeMismatch ("Integer", otherB))
    | x -> NumArgs (2, List.length x) |> LispError

let strBoolBinOp fn args = LispError (NotImplemented "sorry")
let boolBoolBinOp fn args = LispError (NotImplemented "sorry")

let primitives = Map [("+", numericOperator (+));
                      ("-", numericOperator (-));
                      ("*", numericOperator (*));
                      ("/", numericOperator (/));
                      ("mod", numericOperator (%));
                      //("quotient", numericOperator quot);
                      //("remainder", numericOperator rem);                       
                       ("=", numBoolBinOp (=));
                       ("<", numBoolBinOp (<));
                       (">", numBoolBinOp (>));
                       ("/=", numBoolBinOp (<>));
                       (">=", numBoolBinOp (>=));
                       ("<=", numBoolBinOp (<=));
                       // ("&&", boolBoolBinOp (&&));
                       // ("||", boolBoolBinOp (||));
                       // ("string=?", strBoolBinOp (==));
                       // ("string<?", strBoolBinOp (<));
                       // ("string>?", strBoolBinOp (>));
                       // ("string<=?", strBoolBinOp (<=));
                       // ("string>=?", strBoolBinOp (>=));
                       ]
// Evaluation
let apply f args =
  match Map.tryFind f primitives with
    | Some fn -> (fn args)
    | None    -> LispError (NotFunction f)

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
    | Failure(errorMsg,_,_) -> printfn "%s" (showErr (ParseError errorMsg))
    | Success(result,_,_)   ->
      match (eval result) with
        | LispVal v   -> printfn "%s" (showVal v)
        | LispError e -> printfn "%s" (showErr e)

