open Eval

let%test "A variable by itself is free" =
  is_free (make_var "x") (Variable "x") = true

let%test "A variable is free when applied to a function" =
  is_free (make_var "x") (App ((Lambda ("x", Variable "x")), Variable "x")) = true

let%test "A variable is free when applied to an expression" =
  is_free (make_var "x") (App (Variable "w", Variable "x")) = true

let%test "A variable is free when unbound in the body of a lambda" =
  is_free (make_var "x") (Lambda ("w", Variable "x")) = true

let%test "A variable is not free when bound in the variable of a lambda" =
  is_free (make_var "x") (Lambda ("x", Variable "x")) = false

let%test "A variable is not free when bound in the variable of a lambda" =
  is_free (make_var "x") (Lambda ("x", Variable "w")) = false

let%test "Can substitute a variable for another" =
  substitute (make_var "x") (Variable "y") (Variable "x") = Variable "y"

let%test "Can substitute a variable for another in lambda" =
  substitute (make_var "x") (Variable "y") (Lambda ("w", Variable "x")) = Lambda ("w", Variable "y")
