module scheve.Testing

#I "../packages/FParsec.0.9.2.0/lib/net40"
#r "FParsec.dll"
#r "FParsecCS.dll"

#load "Types.fs"
#load "Parser.fs"
#load "Eval.fs"

open FParsec
open scheve.Types
open scheve.Parser
open scheve.Eval

open System

let test p str =
  match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseViewAst str = test parseExpr str

let evalAndPrint env str =
  match run parseExpr str with
    | Failure(errorMsg,_,_) -> do printfn "%s" (showErr (ParseError errorMsg))
                               env
    | Success(result,_,_)   ->
      match (eval env result) with
        | (env, LispVal v)   -> do printfn "=> %s" (showVal v)
                                env
        | (env, LispError e) -> do printfn "%s" (showErr e)
                                env
