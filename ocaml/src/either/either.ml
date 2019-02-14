type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

let pure x = Right x

let fmap f x =
  match x with
  | Left e -> Left e
  | Right v -> Right (f v)

let bimap f1 f2 x =
  match x with
  | Left x' -> Left (f1 x')
  | Right x' -> Right (f2 x')

let splat f x =
  match f with
  | Left e   -> Left e
  | Right f' -> fmap f' x
