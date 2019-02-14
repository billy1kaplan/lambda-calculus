(* Lexer *)
type var = string

type func = var * expr
and expr =
  | Variable of var
  | Lambda of func
  | App of expr * expr

let substitute var value exp =
  let rec aux exp =
    match exp with
    | Variable v' ->
      if var = v'
      then value
      else Variable v'
    | Lambda (v', body) ->
      if var <> v'
      then Lambda (v', aux body)
      else Lambda (v', body)
    | App (e1, e2) -> App (aux e1, aux e2)
  in aux exp

let rec to_string exp =
  match exp with
  | Variable v -> v
  | Lambda (v, e) -> "\\" ^ v ^ "." ^ to_string e
  | App(e1, e2) -> "(" ^ to_string e1 ^ " " ^ to_string e2 ^ ")"

let rec equal e1 e2 =
  match e1, e2 with
  | Variable v1, Variable v2 -> v1 = v2
  | Lambda (v1, e1), Lambda (v2, e2) -> v1 = v2 && equal e1 e2
  | App(e1, e2), App(e1', e2') -> equal e1 e1' && equal e2 e2'
  | _, _ -> false

let rec eval exp =
  let rec force exp =
    match exp with
    | Variable v' -> Variable v'
    | Lambda (v', body) -> Lambda (v', force body)
    | App (e1, e2) ->
      iter (App (e1, e2))
  and iter exp =
    match exp with
    | Variable v' -> Variable v'
    | Lambda (v', body) -> Lambda (v', body)
    | App (e1, e2) ->
      let e1' = iter e1 in
      match e1' with
      | Lambda (v', body) -> iter (substitute v' e2 body)
      | x -> App (x, iter e2)
  in
  let red = iter exp in
  let red' = force red in
  if equal red red'
  then red'
  else eval red'
