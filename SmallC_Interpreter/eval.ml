open Types
open Utils
open List

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env e = 
	match e with
	| Int a -> Int_Val a
	| Bool a -> Bool_Val a
	| ID a -> (if mem_assoc a env then assoc a env else raise (DeclareError ""))
	| Add (a,b) -> (match (a,b) with		  
		   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
				| (Int_Val c, Int_Val d) -> Int_Val (c+d)
				| (e,f) -> raise (TypeError "")))		   
	| Sub (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Int_Val (c-d)
                                | (e,f) -> raise (TypeError "")))
	| Mult (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Int_Val (c*d)
                                | (e,f) -> raise (TypeError "")))
	| Div (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) when d = 0 -> raise DivByZeroError
				| (Int_Val c, Int_Val d) -> Int_Val (c/d)
                                | (e,f) -> raise (TypeError "")))
	| Pow (a,b) -> (match (a,b) with
		   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Int_Val (int_of_float ((float_of_int c)**(float_of_int d)))
                                | (e,f) -> raise (TypeError "")))
	| Or (a,b) -> (match (a,b) with
		   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Bool_Val c, Bool_Val d) -> Bool_Val (c||d)
                                | (e,f) -> raise (TypeError "")))
        | And (a,b) -> (match (a,b) with
		   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Bool_Val c, Bool_Val d) -> Bool_Val (c&&d)
                                | (e,f) -> raise (TypeError "")))
        | Not a -> (match eval_expr env a with
                   | Bool_Val x -> Bool_Val (not x)
                   | _ -> raise (TypeError ""))
        | Greater (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Bool_Val (c > d)
                                | (e,f) -> raise (TypeError "")))
        | Less (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Bool_Val (c < d)
                                | (e,f) -> raise (TypeError "")))
        | GreaterEqual (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Bool_Val (c >= d)
                                | (e,f) -> raise (TypeError "")))
        | LessEqual (a,b) -> (match (a,b) with
		   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Bool_Val (c <= d)
                                | (e,f) -> raise (TypeError "")))
        | Equal (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Bool_Val (c = d)
                                | (Bool_Val c, Bool_Val d) -> Bool_Val (c = d)
                                | (e,f) -> raise (TypeError "")))
        | NotEqual (a,b) -> (match (a,b) with
                   | (x,y) -> (match (eval_expr env x, eval_expr env y) with
                                | (Int_Val c, Int_Val d) -> Bool_Val (c <> d)
				| (Bool_Val c, Bool_Val d) -> Bool_Val (c <> d)
                                | (e,f) -> raise (TypeError "")))


let rec eval_stmt env s = 
	match s with
	| NoOp -> env
	| Seq (s1, s2) -> eval_stmt (eval_stmt env s1) s2
	| Declare (dt, str) -> (if (mem_assoc str env) then raise (DeclareError "")
				else match dt with
			    		| Int_Type -> (str, Int_Val 0)::env
			    		| Bool_Type -> (str, Bool_Val false)::env)
	| Assign (str, expr) -> (if (mem_assoc str env) then
					match (assoc str env,eval_expr env expr) with
					| (Int_Val _, Int_Val a) -> ((str, Int_Val a)::(remove_assoc str env))
					| (Bool_Val _, Bool_Val a) -> ((str, Bool_Val a)::(remove_assoc str env))
					| _ -> raise (TypeError "")
                                else raise (DeclareError ""))
	| If (expr, s1, s2) -> (match (eval_expr env expr) with
			  	| Bool_Val true -> (eval_stmt env s1)
				| Bool_Val false -> (eval_stmt env s2)
				| _ -> raise (TypeError ""))
	| While (expr, s) -> (match (eval_expr env expr) with
                                | Bool_Val true -> eval_stmt (eval_stmt env s) (While (expr, s))
				| Bool_Val false -> env
				| _ -> raise (TypeError ""))
	| Print expr -> (match (eval_expr env expr) with
			| Int_Val a -> (let _ = (print_output_int a) in let _ = (print_output_string "\n") in env)
			| Bool_Val a -> (let _ = (print_output_bool a) in let _ = (print_output_string "\n") in env))
