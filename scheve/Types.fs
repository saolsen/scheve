module scheve.Types

open System

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
  | NumArgs of int * int
  | NotImplemented of string

type MaybeLispVal =
  | LispVal of LispVal
  | LispError of LispError

exception NotALispVal
exception NotALispError
exception UncheckedType

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

let showErr error =
  "Error: " +
  match error with
    | TypeMismatch (expected, value) -> "Type Mismatch\n" +
                                        "Expected: " + expected + "\n" +
                                        "Got: " + (showVal value)
    | ParseError error -> "ParseError\n" + error
    | NotFunction f -> "Value is not a function: " + f
    | NumArgs (needed, got) -> "Wrong number of arguments\n" +
                               "Needed: " + needed.ToString() +
                               "Got: " + got.ToString()
    | NotImplemented f -> "Functionality is not implemented: " + f
