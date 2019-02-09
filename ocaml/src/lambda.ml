open Parser

(* Grammer :
<expr> ::= (\<var> <expr>)
         | (<expr> <expr>)
         | <var>

<var> ::= s
*)

let rec display exp =
  match exp with
  | Eval.Variable v -> v
  | Lambda (v, e) -> "\\" ^ v ^ "." ^ display e
  | App(e1, e2) -> "(" ^ display e1 ^ " " ^ display e2 ^ ")"

let implode l = String.concat "" (List.map (String.make 1) l)

let explode s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let var = alpha

let rec lambda () =
  chr '\\' >>= fun _ ->
  var >>= fun v ->
  chr '.' >>= fun _ ->
  expr () >>= fun e ->
  return (Eval.Lambda (String.make 1 v, e))
and app () = parens (empty >>= fun _ -> (* Temporary hack to block eval of expr () leading to infinite recursion*)
                    expr () >>= fun e1 ->
                    space >>= fun _ ->
                    expr () >>= fun e2 ->
                    return (Eval.App (e1, e2)))
and expr () = (lambda ())
           <|> fmap (fun c -> c |> String.make 1 |> fun x -> Eval.Variable x) alpha
           <|> app ()

let parser s = let parsed_expr = (expr ()) s in
  match parsed_expr with
  | Error err -> Error ("Parse error: " ^ err)
  | result    -> result

let read_lines (name : string) : string list =
  let ic = open_in name in
  let read_line () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match read_line () with
    | Some s -> loop (s :: acc)
    | None   -> close_in ic; List.rev acc in
  loop []

let eval s =
  match s with
  | Right (e, _) -> Right (display (Eval.eval e))
  | Error err    -> Error err

let print_result r =
  match r with
  | Right s
  | Error s -> print_endline s

let from_source () = String.concat "" (read_lines "source.txt")
                     |> explode
                     |> parser
                     |> eval

let repl () =
  let rec loop () =
    print_string "> ";
    let line = read_line () in
    line
    |> explode
    |> parser
    |> eval
    |> print_result
    |> fun _ -> loop () in
  loop ()
