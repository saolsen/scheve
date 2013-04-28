module scheve.Eval

open scheve.Types

let numericOperator fn args =
  match List.tryFind (fun x -> not (isInteger x)) args with
    | Some nonInt -> LispError (TypeMismatch ("Integer", nonInt))
    | None ->
      List.reduce fn (List.map unpackInteger args) |> Integer |> LispVal

let numBoolBinOp fn args =
  match args with
    | [Integer a; Integer b] -> (fn a b) |> Bool |> LispVal
    | [otherA; Integer _] -> LispError (TypeMismatch ("Integer", otherA))
    | [_; otherB]         -> LispError (TypeMismatch ("Integer", otherB))
    | x                   -> NumArgs (2, List.length x) |> LispError

let strBoolBinOp fn args =
  match args with
    | [String a; String b] -> (fn a b) |> Bool |> LispVal
    | [otherA; String _]   -> LispError (TypeMismatch ("String", otherA))
    | [_; otherB]          -> LispError (TypeMismatch ("String", otherB))
    | x                    -> NumArgs (2, List.length x) |> LispError
    
let boolBoolBinOp fn args =
  match args with
    | [Bool a; Bool b] -> (fn a b) |> Bool |> LispVal
    | [otherA; Bool _]   -> LispError (TypeMismatch ("Bool", otherA))
    | [_; otherB]          -> LispError (TypeMismatch ("Bool", otherB))
    | x                    -> NumArgs (2, List.length x) |> LispError

let car args =
  match args with
    | [List (head :: tail)]               -> head |> LispVal
    | [DottedList ((head :: tail), last)] -> head |> LispVal
    | [item] -> LispError (TypeMismatch ("List", item))
    | x      -> NumArgs (1, List.length x) |> LispError

let cdr args =
  match args with
    | [List (head :: tail)] -> List tail |> LispVal
    // todo nil?
    | [DottedList ([_], last)] -> last |> LispVal
    | [DottedList ((head :: tail), last)] -> DottedList (tail, last) |> LispVal
    | [item] -> LispError (TypeMismatch ("List", item))
    | x      -> NumArgs (1, List.length x) |> LispError

let cons args =
  match args with
    | [x; List []] -> List [x] |> LispVal
    | [x; List xs] -> List (x :: xs) |> LispVal
    | [x; DottedList (xs, last)] -> DottedList ((x :: xs), last) |> LispVal
    | [x1; x2] -> DottedList ([x1], x2) |> LispVal
    | x -> NumArgs (2, List.length x) |> LispError

let rec eqv args =  
  match args with
    | [Bool a; Bool b] -> Bool (a = b) |> LispVal
    | [Integer a; Integer b] -> Bool (a = b) |> LispVal
    | [String a; String b] -> Bool (a = b) |> LispVal
    | [Atom a; Atom b] -> Bool (a = b) |> LispVal
    | [DottedList (xs, x); DottedList (ys, y)] ->
      eqv [List (List.append xs [x]); List (List.append ys [y])]
    | [List x; List y] ->
      if (List.length x) = (List.length y) then
        let eqvs = List.map2 (fun a b -> eqv [a; b]) x y in
          match List.tryFind isLispError eqvs with
            | Some e -> e
            | None   ->
              // if not error, eqv only returns bools
              List.map getLispVal eqvs
              |> List.map unpackBool
              |> List.fold (&&) true
              |> Bool |> LispVal
      else
        Bool false |> LispVal
    | [_; _] -> Bool false |> LispVal
    | x -> NumArgs (2, List.length x) |> LispError

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
                      ("&&", boolBoolBinOp (&&));
                      ("||", boolBoolBinOp (||));
                      ("string=?", strBoolBinOp (=));
                      ("string<?", strBoolBinOp (<));
                      ("string>?", strBoolBinOp (>));
                      ("string<=?", strBoolBinOp (<=));
                      ("string>=?", strBoolBinOp (>=));
                      ("car", car);
                      ("cdr", cdr);
                      ("cons", cons);
                      ("eq?", eqv);
                      ("eqv?", eqv);
                      ]

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
let apply env f args =
  match Map.tryFind f primitives with
    | Some fn -> (env, (fn args))
    | None    -> (env, LispError (NotFunction f))

let rec eval env lisp =
  match lisp with
    //need the rest of these or going to get errors.
    | String _ as value -> (env, LispVal value)
    | Integer _ as value -> (env, LispVal value)
    | Bool _ as value -> (env, LispVal value)
    | Atom id -> (env, getVar env id)
    | List [Atom "quote"; value] -> (env, LispVal value)
    | List [Atom "if"; pred; conseq; alt] ->
      let (newEnv, result) = (eval env pred)
      match result with
        | LispError _ as e -> (newEnv, e)
        | LispVal (Bool false) -> eval newEnv alt
        | _                    -> eval newEnv conseq
    | List [Atom "set!"; Atom var; form] -> setVar var (eval env form)
    | List [Atom "define"; Atom var; form] -> defineVar var (eval env form)
    | List (Atom func :: args) ->
      let evalArgs (currentEnv, evaledArgs) next =
        let (newEvn, evaled) = (eval currentEnv next) in
          (newEvn, (List.append evaledArgs [evaled]))

      let (newEnv, results) = (List.fold evalArgs (env, []) args) in
        if List.forall isLispVal results then
          apply newEnv func (List.map getLispVal results)
        else
          (newEnv, (List.find isLispError results))
          
    | _ -> (env, (NotImplemented "eval of this form" |> LispError))

