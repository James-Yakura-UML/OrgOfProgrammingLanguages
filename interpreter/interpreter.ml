type typ =  
| TBool  
| TInt  
| TArrow of typ * typ  

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

exception Eval_error  
exception Type_error  
exception Substitution_error  

let rec step (e : exp) = 
match e with 
| If (e0, e1, e2) ->  (
  match multi_step e0 with 
  | True -> multi_step (e1) 
  | False -> multi_step (e2) 
  | _ -> raise Eval_error
  ) 
| IsZero (n) ->  
  (match multi_step n with 
  | Num (n1) ->  
    (match n1 with 
    | 0 -> True 
    | n -> False 
    | _ -> raise Eval_error) 
  | _ -> raise Eval_error)
| Plus (e0, e1) ->  
  (match multi_step e0 with 
  | Num (n0) ->  
    (match multi_step e1 with  
    | Num (n1) -> Num (n0 + n1) 
    | _ -> raise Eval_error) 
  | _ -> raise Eval_error) 
| Mult (e0, e1) ->  
  (match multi_step e0 with 
  | Num (n0) ->  
    (match multi_step e1 with 
    | Num (n1) -> Num (n0*n1) 
    | _ -> raise Eval_error) 
  | _ -> raise Eval_error) 
| Apply (e0, e1) ->  
  (match e0 with 
  | Lambda (var, typ, e0) -> multi_step (substitution e0 var e1) 
  | _ -> raise Eval_error) 
| _ -> raise Eval_error  

and multi_step (e: exp) = 
match e with  
| True -> True 
| False -> False 
| Num (n) -> Num (n) 
| Var (name) -> Var (name) 
| Lambda (var, typ, e0) -> Lambda (var, typ, e0) 
| _ -> step e  

and type_check (e: exp) = 
match e with
| True -> TBool 
| False -> TBool 
| If (cond,_then,_else) -> 
  ( match type_check cond with 
  | TBool -> 
    ( match type_check _then with 
    | t -> 
      (match type_check _else with 
      | x when (x=t) -> t
      | _ -> raise Type_error)) 
  | _ -> raise Type_error ) 
| Num (n) -> TInt
| IsZero (e) -> 
  (match type_check e with
  | TInt -> TBool
  | _ -> raise Type_error)
| Plus (left,right) -> 
  (match type_check left with
  | TInt -> 
    (match type_check right with
    | TInt -> TInt
    | _ -> raise Type_error)
  | _ -> raise Type_error)
| Mult (left,right) -> 
  (match type_check left with
  | TInt -> 
    (match type_check right with
    | TInt -> TInt
    | _ -> raise Type_error)
  | _ -> raise Type_error)
| Var (handle) -> raise Type_error
| Lambda (var_name, var_type, expr) -> TArrow (var_type, type_check (substitution expr var_name (dummy_value var_type)))
| Apply (lambda, argument) -> 
  (match type_check lambda with
  | TArrow (lambda_left_type, lambda_right_type) -> 
    (match type_check argument with
    | lambda_left_type -> lambda_right_type
    | _ -> raise Type_error)
  | t -> t)
| _ -> raise Type_error

and dummy_value (t: typ)=(*This is a separate method in order to handle functions that output functions. Dummy values are purely for type checking and should never be evaluated.*) 
match t with  
| TBool -> True 
| TInt -> Num (0) 
| TArrow (left, right) -> Lambda("I want to evaluate but the clapping keeps alerting the type checker", left, dummy_value right) 
| _ -> raise Type_error  

and free_variables (e: exp) = 
match e with 
| Var (name) -> [name] 
| If (e0,e1,e2) -> (free_variables e0) @ (free_variables e1) @ (free_variables e2) 
| IsZero (e0) -> (free_variables e0) 
| Plus (e0, e1) -> (free_variables e0) @ (free_variables e1) 
| Mult (e0, e1) -> (free_variables e0) @ (free_variables e1) 
| Lambda (var, t, e0) -> free_variables (substitution e0 var (dummy_value t)) 
| Apply (e0, e1) -> 
  (match e0 with
  | Lambda (var, t, expr) -> free_variables (substitution e0 var e1)
  | _ -> free_variables e0)
| _ -> []  

and substitution (e1: exp) (x: string) (e2: exp) = 
match e1 with 
| Var (name) ->  
  (match name with 
  | x -> e2 
  | _ -> e1) 
| If (condition, ifTrue, ifFalse) -> (If (substitution condition x e2, substitution ifTrue x e2, substitution ifFalse x e2)) 
| IsZero (arg) -> IsZero (substitution arg x e2) 
| Plus (left, right) -> Plus (substitution left x e2, substitution right x e2) 
| Mult (left, right) -> Mult (substitution left x e2, substitution right x e2) 
| Lambda (var, typ, sube) -> Lambda (var, typ, substitution sube x e2) 
| Apply (left, right) -> Apply (substitution left x e2, substitution right x e2) 
| _ -> e1