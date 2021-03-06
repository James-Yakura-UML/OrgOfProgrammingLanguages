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
  | LambdaRec of string * typ * typ * string * exp

type type_environment = (string * typ) list

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
  | Lambda (var, typ, def) -> multi_step (substitution def var e1) 
  | LambdaRec (label, t_left, t_right, var, def) -> 
    recursive_step [(label, Lambda(var, t_left, def))] (substitution def var (multi_step e1))
  | _ -> multi_step e0) 
| _ -> raise Eval_error  

and multi_step (e: exp) = 
match e with  
| True -> True 
| False -> False 
| Num (n) -> Num (n) 
| Var (name) -> Var (name) 
| Lambda (var, typ, e0) -> Lambda (var, typ, e0) 
| LambdaRec (label, left_type, right_type, var, def) -> LambdaRec (label, left_type, right_type, var, def)
| _ -> step e  

and recursive_step (labels: (string * exp) list) (e:exp) =
match e with
| If (cond, then_val, else_val) ->
  (match recursive_step labels cond with
  | True -> recursive_step labels then_val
  | False -> recursive_step labels else_val
  | _ -> raise Eval_error)
| IsZero (number) ->
  (match recursive_step labels number with
  | Num (0) -> True
  | Num (n) -> False
  | _ -> raise Eval_error)
| Plus (left, right) ->
  (match recursive_step labels left with
  | Num (left_value) ->
    (match recursive_step labels right with
    | Num (right_value) -> Num(left_value + right_value)
    | _ -> raise Eval_error)
  | _ -> raise Eval_error)
| Mult (left, right) ->
  (match recursive_step labels left with
  | Num (left_value) ->
    (match recursive_step labels right with
    | Num (right_value) -> Num(left_value * right_value)
    | _ -> raise Eval_error)
  | _ -> raise Eval_error)
| Apply (funct, param) -> 
  (match funct with
  | Lambda (var, left_type, def) ->
    (match substitution def var (recursive_step labels param) with
    | evaluated_funct -> recursive_step labels evaluated_funct)
  | LambdaRec (label, t_left, t_right, var, def) -> 
    (match push_recursive_function labels label (Lambda(var, t_left, def)) with
    | labels_processed ->
      (match substitution def var (recursive_step labels_processed param) with
      | evaluated_funct -> recursive_step labels_processed evaluated_funct
      )
    )
  | Var (label) -> 
    (match search_recursive_functions labels label with
    | replaced_funct -> 
      if replaced_funct=funct 
      then multi_step funct
      else recursive_step labels (Apply(replaced_funct, recursive_step labels param))
    )
  | _ -> recursive_step labels funct)
| _ -> multi_step e

and search_recursive_functions (labels: (string * exp) list) (target: string) =
match labels with
| [] -> Var(target)
| (current_name,current_definition)::t -> if current_name=target then current_definition else search_recursive_functions t target

and push_recursive_function (current_list: (string * exp) list) (label: string) (def: exp) =
match current_list with 
| [] -> [(label, def)]
| (current_name, current_definition)::t ->
  if current_name=label
  then (label, def)::t
  else (label, def)::(push_recursive_function t label def)

and type_check (te: type_environment) (e: exp) = 
match e with
| True -> TBool 
| False -> TBool 
| If (cond,_then,_else) -> 
  ( match type_check te cond with 
  | TBool -> 
    ( match type_check te _then with 
    | t -> 
      (match type_check te _else with 
      | x when (x=t) -> t
      | _ -> raise Type_error)) 
  | _ -> raise Type_error ) 
| Num (n) -> TInt
| IsZero (e) -> 
  (match type_check te e with
  | TInt -> TBool
  | _ -> raise Type_error)
| Plus (left,right) -> 
  (match type_check te left with
  | TInt -> 
    (match type_check te right with
    | TInt -> TInt
    | _ -> raise Type_error)
  | _ -> raise Type_error)
| Mult (left,right) -> 
  (match type_check te left with
  | TInt -> 
    (match type_check te right with
    | TInt -> TInt
    | _ -> raise Type_error)
  | _ -> raise Type_error)
| Var (handle) -> raise Type_error
| Lambda (var_name, var_type, expr) -> TArrow (var_type, type_check te (substitution expr var_name (dummy_value var_type)))
| Apply (lambda, argument) -> 
  (match type_check te lambda with
  | TArrow (lambda_left_type, lambda_right_type) -> 
    (match type_check te argument with
    | lambda_left_type -> lambda_right_type
    | _ -> raise Type_error)
  | t -> t)
| LambdaRec (label, left_type, right_type, var, expr) -> 
  TArrow (left_type, right_type)
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
  | Lambda (var, t, expr) -> free_variables (substitution expr var e1)
  | LambdaRec (label, t_left, t_right, var, expr) -> free_variables (substitution expr var e1)
  | _ -> free_variables e0)
| LambdaRec (label, t_left, t_right, var, expr) -> free_variables (substitution expr var (dummy_value t_left))
| _ -> []  

and substitution (e1: exp) (x: string) (e2: exp) = 
match e1 with 
| Var (name) -> if name=x then e2 else e1
| If (condition, ifTrue, ifFalse) -> (If (substitution condition x e2, substitution ifTrue x e2, substitution ifFalse x e2)) 
| IsZero (arg) -> IsZero (substitution arg x e2) 
| Plus (left, right) -> Plus (substitution left x e2, substitution right x e2) 
| Mult (left, right) -> Mult (substitution left x e2, substitution right x e2) 
| Lambda (var, typ, sube) -> Lambda (var, typ, substitution sube x e2) 
| Apply (left, right) -> Apply (substitution left x e2, substitution right x e2) 
| LambdaRec (label, t_left, t_right, var, expr) -> LambdaRec (label, t_left, t_right, var, substitution expr x e2)
| _ -> e1