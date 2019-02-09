(* Lexer *)
type token =
  | Var of string
  | Def

type var = string

type make_var = string -> var
let make_var s = s

type func = var * expr
and expr =
  | Variable of var
  | Lambda of func
  | App of expr * expr

type is_equal_var = var -> var -> bool
let is_equal_var v1 v2 = v1 = v2

type free = var -> expr -> bool
let is_free v exp =
  let rec aux exp = match exp with
    | Variable v' -> v = v'
    | Lambda (v', f) -> (v <> v') && aux f
    | App (e1, e2) -> aux e1 || aux e2
  in aux exp

type substitute = var -> expr -> expr
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

let succ = Lambda ("w",
                   Lambda ("y",
                           Lambda ("x",
                                   App (Variable "y",
                                        App
                                          (App (Variable "w", Variable "y"),
                                           Variable "x")))))
let zero = Lambda ("w", Lambda("z", Variable "z"))
let one = Lambda ("w", Lambda("z", App (Variable "w", Variable "z")))

let rec display exp =
  match exp with
  | Variable v -> "Var" ^ v
  | Lambda (v, e) -> "\\" ^ v ^ " " ^ display e
  | App(e1, e2) -> "(" ^ display e1 ^ " " ^ display e2 ^ ")"

type church_to_number = expr -> int option
let church_to_number exp =
  let rec aux s z cur exp =
    match exp with
    | Variable v' ->
      if v' = z
      then (Some cur)
      else None
    | Lambda _ -> None
    | App (e1, e2) ->
      if e1 = Variable s
      then aux s z (cur + 1) e2
      else None
  in
  match exp with
  | Lambda (v1, Lambda (v2, body)) -> aux v1 v2 0 body
  | _ -> None

type number_to_church = int -> expr option
let number_to_church n =
  let s = "w" in
  let z = "z" in
  let rec aux i =
    if i = n
    then Variable z
    else App (Variable s, aux (i + 1))
  in
  if n < 0
  then None
  else Some (Lambda (s, Lambda (z, aux 0)))

type eval = expr -> expr
let rec eval exp =
  match exp with
  | Variable v' -> Variable v'
  | Lambda (v', body) -> Lambda (v', eval body)
  | App (e1, e2) ->
    let e1' = eval e1 in
    print_endline ("EVAL: " ^ (display e1'));
    match e1' with
    | Lambda (v', body) -> eval (substitute v' e2 body)
    | x -> App (x, eval e2)
