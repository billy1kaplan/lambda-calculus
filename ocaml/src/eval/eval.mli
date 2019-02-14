type var = string

type func = var * expr
and expr =
  | Variable of var
  | Lambda of func
  | App of expr * expr

val to_string : expr -> string

val eval : expr -> expr
