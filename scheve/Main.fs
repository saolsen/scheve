module scheve.Main

open System
open FParsec

open scheve.Types
open scheve.Eval
open scheve.Parser

let evalAndPrint env str =
  match run parseExpr str with
    | Failure(errorMsg,_,_) ->
      printfn "%s" (showErr (ParseError errorMsg))
      env
    | Success(result,_,_) ->
      let (newEnv, result) = (eval env result)
      match result with
        | LispVal v   ->
          do printfn "=> %s" (showVal v)
          newEnv
        | LispError e ->
          do printfn "%s" (showErr e)
          env

let rec repl env =
  do printf "Scheve <$^&> "
  let input = Console.ReadLine()
  match input with
    | "quit" -> 0
    | null -> 0
    | input ->
      repl (evalAndPrint env input)

[<EntryPoint>]
let main args =
  match args with
    | [|filename|] ->
      do printf "Running a scheme file not yet implemented"
      0
    | _ -> repl Map.empty

