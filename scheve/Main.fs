module scheve.Main

open System
open FParsec

open scheve.Types
open scheve.Eval
open scheve.Parser

let evalAndPrint str =
  match run parseExpr str with
    | Failure(errorMsg,_,_) -> printfn "%s" (showErr (ParseError errorMsg))
    | Success(result,_,_)   ->
      match (eval result) with
        | LispVal v   -> printfn "=> %s" (showVal v)
        | LispError e -> printfn "%s" (showErr e)

let rec repl () =
  do printf "Scheve <$^&> "
  let input = Console.ReadLine()
  match input with
    | "quit" -> 0
    | null -> 0
    | input ->
      do evalAndPrint input
      repl ()

[<EntryPoint>]
let main args =
  match args with
    | [|filename|] ->
      do printf "Running a scheme file not yet implemented"
      0
    | _ -> repl ()

