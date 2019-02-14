open Parser

(* Grammer :
   <expr> ::= (\<var> <expr>)
         | (<expr> <expr>)
         | <var>

   <VAR> = <expr>

   ! (Query environment)
*)

let const x _ = x

let read_lines (name : string) : string list =
  let ic = open_in name in
  let read_line () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match read_line () with
    | Some s -> loop (s :: acc)
    | None   -> close_in ic; List.rev acc in
  loop []

let explode s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let char_to_string = String.make 1

type exp =
  | Expr of Eval.expr
  | Def of Eval.var * Eval.expr
  | Query

let rec lambda () =
  chr '\\' >>= fun _ ->
  lower >>= fun v ->
  chr '.' >>= fun _ ->
  expr () >>= fun e ->
  return (Eval.Lambda (char_to_string v, e))
and app () = parens (many space >>= fun _ ->
                     expr () >>= fun e1 ->
                     some space >>= fun _ ->
                     expr () >>= fun e2 ->
                     return (Eval.App (e1, e2)))
and variable () = fmap (fun c -> c
                                 |> char_to_string
                                 |> fun x -> (Eval.Variable x)) (lower <|> upper)
and expr () = (lambda ())
              <|> variable ()
              <|> app ()

let query () = fmap (const Query) (chr '!')

let assign () =
  upper >>= fun v ->
  many space >>= fun _ ->
  chr '=' >>= fun _ ->
  many space >>= fun _ ->
  expr () >>= fun e ->
  return (Def (char_to_string v, e))

let parser =
  let grammar = assign ()
                <|> query ()
                <|> fmap (fun x -> Expr x) (expr ()) in
  fun s -> Either.bimap (fun err -> "Parse error: " ^ err) fst (grammar s)

let display exp env =
  let display_def (v, e) = v ^ " = " ^ Eval.to_string e in
  let display_env env =
    let env_prompt = "Currently bound functions:\n" in
    env_prompt ^ String.concat "\n" (List.map display_def env) in
  match exp with
  | Query -> display_env env
  | Def (v, e) -> display_def (v, e)
  | Expr e -> Eval.to_string e

let eval env exp =
  let eval_in_context exp env =
    let make_body (v, e) acc = Eval.App (Eval.Lambda (v, acc), e) in
    Eval.eval (List.fold_right make_body env exp) in
  let update_env v e env =
    let bindings = List.filter (fun (v', _) -> v <> v') !env in
    env := List.append bindings [(v, e)] in
  match exp with
  | Def (v, e) ->
    update_env v e env;
    display exp !env
  | Query  -> display Query !env
  | Expr e -> Eval.to_string (eval_in_context e !env)

let print_either r = ignore (Either.bimap print_endline print_endline r)

let start_repl env =
  let prompt = "> " in
  let rec loop () =
    print_string prompt;
    read_line ()
    |> explode
    |> parser
    |> Either.fmap (eval env)
    |> print_either
    |> loop in
  loop ()

let base_env () = ref []

let repl () = start_repl (base_env ())

let from_source file =
  let env = base_env () in
  read_lines file
  |> List.map explode
  |> List.map parser
  |> List.map (Either.fmap (fun x -> eval env x |> print_endline))
  |> ignore
  |> start_repl env

let main =
  let arg_length = Array.length Sys.argv in
  if arg_length < 2
  then repl ()
  else
    let arg = Sys.argv.(1) in
    if arg = "-h" || arg = "--help"
    then print_endline "
        A lambda calculus interpreter. Execute with no arguments to enter the repl immediately.

        A single argument is for specifying a file to interpret (and possible load definitoins from).
"
    else from_source arg
