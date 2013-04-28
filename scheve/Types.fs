module scheve.Types

open System

type Function = {prms: string list;
                 varargs: string Option;
                 body: LispVal list;
                 closure: Map<string,LispVal>;}

and LispVal =
    | Atom of string
    | Integer of int
    | String of string
    | Bool of bool
    | List of LispVal list
    | DottedList of LispVal list * LispVal
    | PrimitiveFunc of (LispVal list -> MaybeLispVal)
    | Func of Function
    
and LispError =
  | TypeMismatch of string * LispVal
  | ParseError of string
  | NotFunction of LispVal
  | NumArgs of int * int
  | UnboundVar of string
  | NotImplemented of string

and MaybeLispVal =
  | LispVal of LispVal
  | LispError of LispError

exception NotALispVal
exception NotALispError
exception UncheckedType
exception ImpossibleCase

let unpackInteger i =
  match i with
    | Integer i -> i
    | _         -> raise UncheckedType

let unpackBool b =
  match b with
    | Bool b -> b
    | _      -> raise UncheckedType

let isInteger i =
  match i with
    | Integer i -> true
    | _         -> false

let isBool b =
  match b with
    | Bool b -> true
    | _      -> false

let isString s =
  match s with
    | String s -> true
    | _        -> false

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
    | PrimitiveFunc _ -> "<primitive>"
    | Func {prms = prms;
            varargs = varargs;
            body = body;
            closure = closure;} ->
      let args = match varargs with
                   | Some arg -> " . " + arg
                   | None  -> ""
      "(lambda (" + (List.reduce (+) prms) + args + ") ...)"

let showErr error =
  "Error: " +
  match error with
    | TypeMismatch (expected, value) -> "Type Mismatch\n" +
                                        "Expected: " + expected + "\n" +
                                        "Got: " + (showVal value)
    | ParseError error -> "ParseError\n" + error
    | NotFunction f -> "Value is not a function: " + (showVal f)
    | NumArgs (needed, got) -> "Wrong number of arguments\n" +
                               "Needed: " + needed.ToString() +
                               "Got: " + got.ToString()
    | UnboundVar var -> "Unbound Variable: " + var.ToString()
    | NotImplemented f -> "Functionality is not implemented: " + f
