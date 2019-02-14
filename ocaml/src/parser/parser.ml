type input = char list
type 'a parser = input -> (string, ('a * input)) Either.t

let id x = x
let const x _ = x

let fmap f x s = match x s with
  | Either.Left msg -> Either.Left msg
  | Either.Right (x', s') -> Either.Right (f x', s')
let (<$>) = fmap

let pure x s = Either.pure (x, s)

let (<*>) f x s =
  match f s with
  | Either.Left msg -> Either.Left msg
  | Either.Right (f', s') -> (fmap f' x) s'

let (<%) left right = (fmap const left) <*> right
let (%>) left right =
  (pure id <% left) <*> right

let return = pure
let (>>=) p f s = match p s with
  | Either.Left msg       -> Either.Left msg
  | Either.Right (a', s') -> (f a') s'

let (<|>) p1 p2 s =
  match p1 s with
  | Either.Left _  -> p2 s
  | res            -> res


let rec many p =
  (p >>= fun r ->
  many p >>= fun rs ->
  return (r :: rs)) <|> return []

let some p =
  p >>= fun r ->
  many p >>= fun rs ->
  return (r :: rs)

let is_uppercase c =
  match c with
  | 'A' .. 'Z' -> true
  | _          -> false

let is_lowercase c =
  match c with
  | 'a' .. 'z' -> true
  | _          -> false

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _          -> false

let item s =
  match s with
  | []     -> Either.Left "Unexpected end of stream"
  | hd::tl -> Either.Right (hd, tl)

let satisfy p s =
  match item s with
  | Either.Left e -> Either.Left e
  | Right (hd, tl) ->
    if p hd
    then Right (hd, tl)
    else Left "Failed to satisfy"

let empty = pure []
let chr c = satisfy (fun c' -> c = c')
let digit = fmap (fun c -> (Char.code c) - (Char.code '0')) (satisfy is_digit)
let lower = satisfy is_lowercase
let upper = satisfy is_uppercase
let space = chr ' '
let parens p = chr '(' %> p <% chr ')'
