module scheve.Eval

open scheve.Types

let getVar env id =
  match Map.tryFind id env with
    | Some value -> value |> LispVal
    | None -> UnboundVar id |> LispError

let setVar id (env, value) = 
  if isLispError value then
    (env, value)
  else
    let unwrapped = getLispVal value in
    if Map.containsKey id env then
      (Map.add id unwrapped env, value)
    else
      (env, LispError (UnboundVar id))

let defineVar var (env, value) =
  match value with
    | LispError _ -> (env, value)
    | LispVal v -> (Map.add var v env, value)

// Evaluation
let rec apply f args =
  match f with
    | PrimitiveFunc fn -> (fn args)
    | Func {prms = prms;
            varargs = varargs;
            body = body;
            closure = closure;} ->
      if (prms.Length <> args.Length) && (Option.isNone varargs) then
        NumArgs (prms.Length, args.Length) |> LispError
      elif args.Length < prms.Length then
        NumArgs (prms.Length, args.Length) |> LispError
      else

        let (argList, varList) =
          match compare args.Length prms.Length with
            | 0  -> (args, [])
            | 1  -> Seq.toList (Seq.take prms.Length args),
                    Seq.toList (Seq.skip prms.Length args)
            | _  -> raise ImpossibleCase

        let withArgs =
          (List.fold
          (fun state (key, value) -> Map.add key value state)
          closure
          (List.zip prms argList)) in

        let withVarArgs =
          match varargs with
            | Some arg -> Map.add arg (List varList) withArgs
            | None     -> withArgs
        in

        let results = List.map (eval withVarArgs) body in

        match List.tryFind (fun (e, result) -> isLispError result) results with
          | Some (e, error)  -> error
          | None             ->
            match List.head (List.rev results) with
              (e, result) -> result
        
    | _ -> NotFunction f |> LispError

and eval env lisp =
  match lisp with
    | String _ as value -> (env, LispVal value)
    | Integer _ as value -> (env, LispVal value)
    | Bool _ as value -> (env, LispVal value)
    | Atom id -> (env, getVar env id)
    | List [Atom "quote"; value] -> (env, LispVal value)
    | List [Atom "if"; pred; conseq; alt] ->
      let (newEnv, result) = (eval env pred)
      match result with
        | LispError _ as e -> (env, e)
        | LispVal (Bool false) -> eval newEnv alt
        | _                    -> eval newEnv conseq
    | List [Atom "set!"; Atom var; form] -> setVar var (eval env form)
    | List [Atom "define"; Atom var; form] -> defineVar var (eval env form)
    | List (func :: args) ->
      let (e, evalFunc) = eval env func
      match evalFunc with
        | LispError _ as e -> (env, e)
        | LispVal f ->
          let evalArgs (currentEnv, evaledArgs) next =
            let (newEvn, evaled) = (eval currentEnv next) in
              (newEvn, (List.append evaledArgs [evaled]))

          let (newEnv, results) = (List.fold evalArgs (env, []) args) in
            if List.forall isLispVal results then
              (env, apply f (List.map getLispVal results))
            else
              (newEnv, (List.find isLispError results))                          
    | _ -> (env, (NotImplemented "eval of this form" |> LispError))

