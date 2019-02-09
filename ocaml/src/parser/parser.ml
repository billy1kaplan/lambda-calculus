type input = char list

type ('a, 'b) either =
  | Error of 'a
  | Right of 'b


type 'a parser = input -> (string, ('a * input)) either

type ('a, 'b) fmap = ('a -> 'b) -> 'a parser -> 'b parser
let fmap f x s = match x s with
  | Error msg       -> Error msg
  | Right (a', s')  -> Right (f a', s')

type 'a pure = 'a -> 'a parser
let pure x s = Right (x, s)

type ('a, 'b) splat = ('a -> 'b) parser -> 'a parser -> 'b parser
let splat f x s = match f s with
  | Error msg       -> Error msg
  | Right (f', s') -> (fmap f' x) s'

let id x = x
let const x _ = x

let (<%) left right =
  (splat (fmap const left) right)

let (%>) left right =
  splat (pure id <% left) right

type ('a, 'b) bind = 'a parser -> ('a -> 'b parser) -> 'b parser
let bind p f s = match p s with
  | Error msg       -> Error msg
  | Right (a', s') -> (f a') s'
let (>>=) = bind

type 'a alternative = 'a parser -> 'a parser -> 'a parser
let alternative p1 p2 s =
  match p1 s with
  | Error _  -> p2 s
  | res      -> res
let (<|>) = alternative

let return = pure

let is_alpha c =
  match c with
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _          -> false

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _          -> false

let item s =
  match s with
  | []     -> Error "Unexpected end of stream"
  | hd::tl -> Right (hd, tl)

let satisfy p s =
  match item s with
  | Error e -> Error e
  | Right (hd, tl) ->
    if p hd
    then Right (hd, tl)
    else Error "Failed to satisfy"

let empty s = Right ([], s)
let chr c = satisfy (fun c' -> c = c')
let digit = satisfy is_digit
let alpha = satisfy is_alpha
let parens p = chr '(' %> p <% chr ')'
let space = chr ' '
