type exp=
  |True
  |False
  |If of exp * exp * exp
  |Num of int
  |IsZero of exp
  |Plus of exp * exp
  |Mult of exp * exp;;

let rec string_of_exp expr=
match expr with
  |True -> "true"
  |False -> "false"
  |If (e0,e1,e2) -> "if " ^ string_of_exp e0 ^ " then " ^ string_of_exp e1 ^ " else " ^ string_of_exp e2
  |Num (n) -> string_of_int n
  |IsZero (e) -> "isZero " ^ string_of_exp e
  |Plus (e0,e1) -> "(" ^ string_of_exp e0 ^ " + " ^ string_of_exp e1 ^ ")"
  |Mult (e0,e1) -> "(" ^ string_of_exp e0 ^ " * " ^ string_of_exp e1 ^ ")";;

exception Eval_Error;;

let rec eval expr =
  match expr with
    | True -> True
    | False -> False
    | If (e0,e1,e2) -> match eval e0 with
      | True -> eval e1
      | False -> eval e2
      | _ -> raise Eval_Error
    | Num (n) -> Num (n)
    | IsZero (e) -> match eval e with
      | Num (n) -> match n with
        | 0 -> True
        | n -> False
      | _ -> raise Eval_Error
    |Plus (e0,e1) -> match eval e0 with
      | Num (n1) -> match eval e1 with
        | Num (n2) -> Num (n1+n2)
        | _ -> raise Eval_Error
      | _ -> raise Eval_Error
    |Mult (e0,e1) -> match eval e0 with
      | Num (n1) -> match eval e1 with
        | Num (n2) -> Num (n1*n2)
        | _ -> raise Eval_Error
      | _ -> raise Eval_Error


string_of_exp(
  Num(3)
);;
(*  1) 3  *)

string_of_exp(
  True
);;
(*  2) true *)

string_of_exp(
  False
);;
(*  3) false  *)

string_of_exp(
  Plus(
    Num(3),
    Num(2))
);;
(*  4) (3 + 2)  *)

string_of_exp(
  Mult(
    Num(3),
    Num(2)
  )
);;
(*  5) (3 * 2)  *)

string_of_exp(
  Plus(
    Num(3),
    Plus(
      Num(3),
      Mult(
        Num(2),
        Plus(
          Num(3),
          Num(2)))))
);;
(*  6) (3 + (3 + (2 * (3 + 2))))  *)

string_of_exp(
  If(
    True,
    Num(3),
    Num(5))
);;
(*  7) if true then 3 else 5  *)

string_of_exp(
  If(
    False,
    Plus(
      Num(3),
      Num(2)),
    Plus(
      Num(5),
      Num(1)))
);;
(*  8) if false then (3 + 2) else (5 + 1) *)

string_of_exp(
  If(
    Plus(
      False,
      True),
    Plus(
      Num(3),
      False),
    Mult(
      Num(3),
      Num(1)))
);;
(*  9) if (false + true) then (3 + false) else (3 * 1)  *)

string_of_exp(
  If(
    IsZero(
      Num(1)
    ),
    Plus(
      Num(3),
      Num(2)),
    Plus(
      Num(5),
      Num(1)))
);;
(*  10) if (isZero 1) then (3 + 2) else (5 + 1) *)

string_of_exp(
  IsZero(
    Mult(
      Num(3),
      Num(5))
  )
);;
(*  11) (isZero (3 * 5))  *)

string_of_exp(
  IsZero(
    If(
      IsZero(
        Num(1)
      ),
      Plus(
        Num(3),
        Num(2)),
      Plus(
        Num(5),
        Num(1)))
  )
);;
(*  12) (isZero if (isZero 1) then (3 + 2) else (5 + 1))  *)

string_of_exp(
  Plus(
    Num(3),
    If(
      IsZero(
        Num(1)
      ),
      Plus(
        Num(3),
        Num(2)),
      Plus(
        Num(5),
        Num(1))))
);;
(*  13) (3 + if (isZero 1) then (3 + 2) else (5 + 1)) *)

string_of_exp(
  Plus(
    Num(3),
    Mult(
      If(
        IsZero(
          Num(1)
        ),
        Plus(
          Num(3),
          Num(2)),
        Plus(
          Num(5),
          Num(1))),
      IsZero(
        True)))
);;
(*  14) (3 + (if (isZero 1) then (3 + 2) else (5 + 1) * (isZero true))) *)

string_of_exp(
  If(
    If(
      True,
      True,
      False),
    Plus(
      Num(3),
      Num(2)),
    Plus(
      Num(5),
      Num(1)))
);;
(*  15) if if true then true else false then (3 + 2) else (5 + 1) *)

string_of_exp(
  If(
    True,
    If(IsZero(
      Mult(
        Num(3),
        Num(5))
    ),
    Plus(
      Num(3),
      Num(2)),
    Plus(
      Num(5),
      Num(1))),
    If(True,
    Mult(
      Num(3),
      Num(2)),
    Mult(
      Num(2),
      Plus(
        Num(3),
        Num(2)))))
);;
(*  16) if true then if (isZero (3 * 5)) then (3 + 2) else (5 + 1) else if true then (3 * 2) else (2 * (3 + 2)) *)
