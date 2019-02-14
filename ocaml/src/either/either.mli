type ('a, 'b) t =
  | Left of 'a
  | Right of 'b
val pure : 'a -> ('b, 'a) t
val fmap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val bimap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
val splat : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
