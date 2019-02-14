type input = char list
type 'a parser = input -> (string, ('a * input)) Either.t

val fmap : ('a -> 'b) -> 'a parser -> 'b parser
val (<$>) : ('a -> 'b) -> 'a parser -> 'b parser

val pure : 'a -> 'a parser
val (<*>) : ('a -> 'b) parser -> 'a parser -> 'b parser

val (<%) : 'a parser -> 'b parser -> 'a parser

val (%>) : 'a parser -> 'b parser -> 'b parser

val return : 'a -> 'a parser
val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser

val (<|>) : 'a parser -> 'a parser -> 'a parser

val some : 'a parser -> 'a list parser
val many : 'a parser -> 'a list parser

(* Some useful default parsers *)
val empty : 'a list parser
val chr : char -> char parser
val digit : int parser
val lower : char parser
val upper : char parser
val space : char parser
val parens : 'a parser -> 'a parser
