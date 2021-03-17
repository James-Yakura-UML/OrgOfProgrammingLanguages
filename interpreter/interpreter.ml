type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * typ * exp
  | Apply of exp * exp

type typ = 
  | TBool 
  | TInt 
  | TArrow of typ * typ

  exception Eval_error

  exception Type_error

  exception Substitution_error

let rec step (e : exp) =
match e with
  | If (e0, e1, e2) -> match multi_step e0 with
    | True -> multi_step (e1)
    | False -> multi_step (e2)
    | _ -> raise Eval_error
  | IsZero (n) -> match multi_step n with
    | Num (n1) -> match n1 with
      | 0 -> True
      | _ => False
    | _ -> raise Eval_error
  | Plus (e0, e1) -> match multi_step e0 with
    | Num (n0) -> match multi_step e1 with 
      | Num (n1) -> Num (n0+n1)
      | _ -> raise Eval_error
    | _ -> raise Eval_error
  | Mult (e0, e1) -> match multi_step e0 with
    | Num (n0) -> match multi_step e1 with
      | Num (n1) -> Num (n0*n1)
      | _ -> (n0*n1)
    | _ -> raise Eval_error
  | Apply (e0, e1) -> match e0 with
    | Lambda (var, typ, e0) -> multi_step (substitution (e0, var, e1))
    | _ -> raise Eval_error
  | _ -> raise Eval_error

let rec multi_step (e: exp) =
match e with 
  | True -> True
  | False -> False
  | Num (n) -> Num (n)
  | Var (name) -> Var (name)
  | Lambda (var, typ, e0) -> Lambda (var, typ, e0)
  | _ -> step e

let rec type_check (e: exp) =
match e with
  | True -> TBool
  | False -> TBool
  | Num -> TInt
  | If (e0, e1, e2) -> match type_check e0 with
    | TBool -> match type_check e1 with
      | type_check e2 -> type_check e1
      | _ -> raise Type_error
    | _ -> raise Type_error
  | IsZero (e0) -> match type_check e0 with
    | TInt -> TBool
    | _ -> raise Type_error
  | Plus (e0, e1) -> match type_check e0 with
    | TInt -> match type_check e1 with
      | TInt -> TInt
      | _ -> raise Type_error
    | _ -> raise Type_error
  | Mult (e0, e1) -> match type_check e0 with
    | TInt -> match type_check e1 with
      | TInt -> TInt
      | _ -> raise Type_error
    | _ -> raise Type_error
  | Var (name) -> raise Type_error
  | Lambda (var, t, e0) -> TArrow (t, type_check(substitution(e0, var, dummy_value(t))))
  | Apply (e0, e1) -> match type_check e0 with 
    | TArrow (left, right) -> match left with
      | (type_check e1) -> right
      | _ -> raise Type_error
    | _ -> raise Type_error
  | _ -> raise Type_error

let rec dummy_value (t: typ)=match t with (*This is a separate method in order to handle functions that output functions.*)
  | TBool -> True
  | TInt -> Num (0)
  | TArrow (left, right) -> Lambda("X", left, dummy_value (right))
  | _ -> raise Type_error

let rec free_variables (e: exp) = match e with
  | Var (name) -> name ^ ", "
  | If (e0,e1,e2) -> (free_variables e0) ^ (free_variables e1) ^ (free_variables e2)
  | IsZero (e0) -> (free_variables e0)
  | Plus (e0, e1) -> (free_variables e0) ^ (free_variables e1)
  | Mult (e0, e1) -> (free_variables e0) ^ (free_variables e1)
  | Lambda (var, t, e0) -> free_variables (substitution (e0, var, dummy_value (t)))
  | Apply (e0, e1) -> free_variables (substitution (e0, var, e1))
  | _ -> ""

let rec substitution (e1: exp) (x: string) (e2: exp) = match e1 with
  | Var (name) -> match name with
    | x -> e2
    | _ -> e1
  | If (condition, ifTrue, ifFalse) -> If (substitution condition, substitution ifTrue, substitution ifFalse)
  | IsZero (arg) -> IsZero (substitution arg)
  | Plus (left, right) -> Plus (substitution left, substitution right)
  | Mult (left, right) -> Mult (substitution left, substitution right)
  | Lambda (var, typ, sube) -> Lambda (var, typ, substitution sube)
  | Apply (left, right) -> Apply (substitution left, substitution right)
  | _ -> e1