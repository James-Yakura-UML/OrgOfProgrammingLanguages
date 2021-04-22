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
  | Lambda of string * exp
  | Apply of exp * exp
  | Let of string * exp * exp

type type_environment = (string * typ) list
type environment = (string * exp) list

exception Eval_error  
exception Type_error  
exception Substitution_error  

let rec step (env : environment) (e : exp) : (environment * exp) = match e with
| If (cond, then_exp, else_exp) -> 
  (match multi_step env cond with
  | (en, cond_eval) -> 
    (match cond_eval with
      | True -> multi_step env then_exp
      | False -> multi_step env else_exp
      | _ -> raise Type_error
    )
  | _ -> raise Eval_error
  )
| IsZero (number) -> 
  (match multi_step env number with
  | (en, number_eval) -> 
    (match number_eval with
    | Num (value) ->
      (match value with 
        | 0 -> (en, True)
        | _ -> (en, False)
      )
    | _ -> raise Type_error
    )
  | _ -> raise Eval_error
  )
| Plus (left, right) ->
  (match multi_step env left with
  | (en, left_eval) ->
    (match left_eval with
    | Num (left_val) -> 
      (match multi_step en right with
      | (en1, right_eval) ->
        (match right_eval with
        | Num (right_val) -> (en1, Num(left_val+right_val))
        | _ -> raise Type_error
        )
      | _ -> raise Eval_error
      )
    | _ -> raise Type_error
    )
  | _ -> raise Type_error
  )
| Mult (left, right) ->
  (match multi_step env left with
  | (en, left_eval) ->
    (match left_eval with
    | Num (left_val) -> 
      (match multi_step en right with
      | (en1, right_eval) ->
        (match right_eval with
        | Num (right_val) -> (en1, Num(left_val*right_val))
        | _ -> raise Type_error
        )
      | _ -> raise Eval_error
      )
    | _ -> raise Type_error
    )
  | _ -> raise Type_error
  )
| Apply (expression, input) ->
  (match multi_step env expression with
  | (en,Lambda (var, expr)) -> step en (Let(var, input, expr))
  | _ -> multi_step env expression
  )
| Var (name) -> multi_step env (lookup_variable env name)
| Let (var, input, expression) -> multi_step (update_environment [] env var input) expression
| _ -> raise Eval_error

and lookup_variable (env:environment) (name:string) : exp = match env with
| (current_name, current_value)::tail ->
  if current_name=name
    then current_value
  else lookup_variable tail name
| [] -> raise Eval_error

and update_environment (env_h:environment) (env_t:environment) (name:string) (value:exp) : environment = match env_t with
| (current_name, current_value)::tail ->
  if current_name=name
    then
      match multi_step (env_h@env_t) value with
      | (en, value_eval) -> (name, value_eval)::env_h@env_t
      | _ -> raise Eval_error
  else update_environment ((current_name, current_value)::env_h) tail name value
| [] -> 
  match multi_step env_h value with
  | (en, value_eval) -> (name, value_eval)::env_h
  | _ -> raise Eval_error

and multi_step (env : environment) (e : exp) : (environment * exp) = match e with
| True -> (env, e)
| False -> (env, e)
| Num (value) -> (env, e) 
| Lambda(var, expr) -> (env,e)
| _ -> step env e

and type_check (te:type_environment) (e:exp) : (type_environment * typ) = match e with 
| True -> (te, TBool)
| False -> (te, TBool)
| If (cond, then_exp, else_exp) ->
  (match type_check te cond with
  | (en, TBool) ->
    (match type_check te then_exp with
    | (en, then_type) ->
      (match type_check te else_exp with
      | (en, else_type) -> 
        if then_type = else_type
        then (en, then_type)
        else raise Type_error
      | _ -> raise Eval_error)
    | _ -> raise Eval_error)
  | _ -> raise Type_error)
| Num (value) -> (te, TInt)
| IsZero (number) -> 
  (match type_check te number with
  | (en, TInt) -> (te, TInt)
  | _ -> raise Type_error)
| Plus (left, right) ->
  (match type_check te left with
  | (en, TInt) -> 
    (match type_check te right with
    | (en, TInt) -> (te, TInt)
    | _ -> raise Type_error)
  | _ -> raise Type_error)
| Mult (left, right) ->
  (match type_check te left with
  | (en, TInt) -> 
    (match type_check te right with
    | (en, TInt) -> (te, TInt)
    | _ -> raise Type_error)
  | _ -> raise Type_error)
| Var (name) -> raise Type_error (*Search for variable in type environment*)
| Lambda (name, expression) -> raise Type_error (*Evaluate with dummy variables?*)
| Apply (expression, input) -> raise Type_error (*Run Let on it*)
| Let (name, input, expression) -> raise Type_error (*Recursive evaluation*)
| _ -> raise Type_error

and dummy_value (t: typ)=(*This is a separate method in order to handle functions that output functions. Dummy values are purely for type checking and should never be evaluated.*) 
match t with  
| TBool -> True 
| TInt -> Num (0) 
| TArrow (left, right) -> Lambda("I want to evaluate but the clapping keeps alerting the type checker", dummy_value right) 
| _ -> raise Type_error  