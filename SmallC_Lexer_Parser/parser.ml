open SmallCTypes
open Utils

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = 
  match toks with
  | [] -> raise (InvalidInputException "No tokens")
  | (a::b) -> a

let rec parse_or toks = 
  let (toks1,a1) = parse_and toks in
  let t = lookahead toks1 in
  match t with
  | Tok_Or -> (let t2 = match_token toks1 Tok_Or in
	      let (toks2,a2) = parse_or t2 in
	      (toks2, Or(a1,a2)))
  | _ -> (toks1,a1)

and parse_and toks =
  let (toks1,a1) = parse_eq toks in
  let t = lookahead toks1 in
  match t with
  | Tok_And -> (let t2 = match_token toks1 Tok_And in
	       let (toks2,a2) = parse_and t2 in
	       (toks2,And(a1,a2)))
  | _ -> (toks1,a1)

and parse_eq toks =
  let (toks1,a1) = parse_rel toks in
  let t = lookahead toks1 in
  match t with
  | Tok_Equal -> (let t2 = match_token toks1 Tok_Equal in
                 let (toks2,a2) = parse_eq t2 in
                 (toks2,Equal(a1,a2)))
  | Tok_NotEqual -> (let t2 = match_token toks1 Tok_NotEqual in
                 let (toks2,a2) = parse_eq t2 in
                 (toks2,NotEqual(a1,a2)))
  | _ -> (toks1,a1)

and parse_rel toks =
  let (toks1,a1) = parse_add toks in
  let t = lookahead toks1 in
  match t with
  | Tok_Less -> (let t2 = match_token toks1 Tok_Less in
                let (toks2,a2) = parse_rel t2 in
                (toks2,Less(a1,a2)))
  | Tok_Greater -> (let t2 = match_token toks1 Tok_Greater in
                   let (toks2,a2) = parse_rel t2 in
                   (toks2,Greater(a1,a2)))
  | Tok_LessEqual -> (let t2 = match_token toks1 Tok_LessEqual in
		     let (toks2,a2) = parse_rel t2 in
                     (toks2,LessEqual(a1,a2)))
  | Tok_GreaterEqual -> (let t2 = match_token toks1 Tok_GreaterEqual in
                	let (toks2,a2) = parse_rel t2 in
                        (toks2,GreaterEqual(a1,a2)))
  | _ -> (toks1,a1)

and parse_add toks =
  let (toks1,a1) = parse_mult toks in
  let t = lookahead toks1 in
  match t with
  | Tok_Add -> (let t2 = match_token toks1 Tok_Add in
                 let (toks2,a2) = parse_add t2 in
                 (toks2,Add(a1,a2)))
  | Tok_Sub -> (let t2 = match_token toks1 Tok_Sub in
                 let (toks2,a2) = parse_add t2 in
                 (toks2,Sub(a1,a2)))
  | _ -> (toks1,a1)

and parse_mult toks =
  let (toks1,a1) = parse_pow toks in
  let t = lookahead toks1 in
  match t with
  | Tok_Mult -> (let t2 = match_token toks1 Tok_Mult in
                 let (toks2,a2) = parse_mult t2 in
                 (toks2,Mult(a1,a2)))
  | Tok_Div -> (let t2 = match_token toks1 Tok_Div in
                 let (toks2,a2) = parse_mult t2 in
                 (toks2,Div(a1,a2)))
  | _ -> (toks1,a1)

and parse_pow toks =
  let (toks1,a1) = parse_unary toks in
  let t = lookahead toks1 in
  match t with
  | Tok_Pow -> (let t2 = match_token toks1 Tok_Pow in
                 let (toks2,a2) = parse_pow t2 in
                 (toks2,Pow(a1,a2)))
  | _ -> (toks1,a1)

and parse_unary toks =
  let (toks1,a1) = parse_primary toks in
  let t = lookahead toks in
  match t with
  | Tok_Not -> (let t2 = match_token toks Tok_Not in
                 let (toks2,a2) = parse_primary t2 in
                 (toks2,Not(a2)))
  | _ -> (toks1,a1)

and parse_primary toks =
  let t = lookahead toks in
  match t with
  | Tok_Int n -> (let t2 = match_token toks (Tok_Int n) in
                 (t2,Int n))
  | Tok_Bool n -> (let t2 = match_token toks (Tok_Bool n) in
                 (t2,Bool n))
  | Tok_ID n -> (let t2 = match_token toks (Tok_ID n) in
                 (t2,ID n))
  | Tok_LParen -> let t2 = match_token toks Tok_LParen in
		let (toks1,a1) = parse_or t2 in
		let t3 = match_token toks1 Tok_RParen in
		(t3, a1)
  | _ -> raise (InvalidInputException "parse_primary error")

let rec parse_expr toks = 
  parse_or toks

let rec parse_stmt toks =
  if toks = [] then (toks, NoOp)
  else
    let t = lookahead toks in
    match t with
    | Tok_Int_Type ->	let t2 = match_token toks Tok_Int_Type in
			let (toks1,a1) = int_declare t2 in
			let (toks2,a2) = parse_stmt toks1 in
			(toks2, Seq(a1,a2))
    | Tok_Bool_Type -> 	let t2 = match_token toks Tok_Bool_Type in
			let (toks1,a1) = bool_declare t2 in
			let (toks2,a2) = parse_stmt toks1 in
			(toks2, Seq(a1,a2))
    | Tok_ID x ->	let (toks1,a1) = assign toks in
			let (toks2,a2) = parse_stmt toks1 in
			(toks2, Seq(a1,a2))
    | Tok_Print ->	let t2 = match_token toks Tok_Print in
			let (toks1,a1) = print t2 in
			let (toks2,a2) = parse_stmt toks1 in
			(toks2, Seq(a1,a2))
    | Tok_If ->		let t2 = match_token toks Tok_If in
			let t3 = match_token t2 Tok_LParen in
			let (toks1,a1) = if_stmt t3 in
			let (toks2,a2) = parse_stmt toks1 in
			(toks2, Seq(a1,a2))
    | Tok_While ->	let t2 = match_token toks Tok_While in
			let (toks1,a1) = while_stmt t2 in
			let (toks2,a2) = parse_stmt toks1 in
			(toks2, Seq(a1,a2))
    | _ ->		(toks, NoOp)

and int_declare toks =
  let t = lookahead toks in
  match t with 
  | Tok_ID x ->	let t1 = match_token toks (Tok_ID x) in
	  	let t2 = match_token t1 Tok_Semi in
	  	(t2, Declare (Int_Type, x))
  | _ -> raise (InvalidInputException "declare")

and bool_declare toks = 
  let t = lookahead toks in
  match t with
  | Tok_ID x ->	let t1 = match_token toks (Tok_ID x) in
	  	let t2 = match_token t1 Tok_Semi in
	  	(t2, Declare (Bool_Type, x))
  | _ -> raise (InvalidInputException "declare")

and assign toks =
  let t = lookahead toks in
  match t with
  | Tok_ID x ->	let t1 = match_token toks (Tok_ID x) in
	  	let t2 = match_token t1 Tok_Assign in
	  	let (toks1,a1) = parse_expr t2 in
	  	let t3 = match_token toks1 Tok_Semi in
	  	(t3, Assign (x, a1))
  | _ -> raise (InvalidInputException "assign")

and print toks =
  let t = lookahead toks in
  match t with
  | Tok_LParen -> let t1 = match_token toks Tok_LParen in
	  	let (toks1,a1) = parse_expr t1 in
	  	let t2 = match_token toks1 Tok_RParen in
	  	let t3 = match_token t2 Tok_Semi in
	  	(t3, Print a1)
  | _ -> raise (InvalidInputException "print")

and if_stmt toks =  	
  let (toks1,a1) = parse_expr toks in
  let t1 = match_token toks1 Tok_RParen in
  let t2 = match_token t1 Tok_LBrace in
  let (toks2,a2) = parse_stmt t2 in
  let t3 = match_token toks2 Tok_RBrace in
  let t4 = lookahead t3 in
  match t4 with
  | Tok_Else -> let t5 = match_token t3 Tok_Else in
  	  	let t6 = match_token t5 Tok_LBrace in
  	  	let (toks3,a3) = parse_stmt t6 in
  	  	let t7 = match_token toks3 Tok_RBrace in
  	  	(t7, If(a1,a2,a3)) 
  | _ -> (t3, If(a1,a2,NoOp))


and while_stmt toks =
  let t = lookahead toks in
  match t with 
  | Tok_LParen -> let t1 = match_token toks Tok_LParen in
	  	let (toks1,a1) = parse_expr t1 in
	  	let t2 = match_token toks1 Tok_RParen in
	  	let t3 = match_token t2 Tok_LBrace in
	  	let (toks2,a2) = parse_stmt t3 in
	  	let t4 = match_token toks2 Tok_RBrace in
	  	(t4, While(a1,a2))
  | _ -> raise (InvalidInputException "while")


let parse_main toks =
  let t = lookahead toks in 
  match t with
  | EOF -> NoOp
  | _ ->
  let toks1 = match_token toks Tok_Int_Type in
  let toks2 = match_token toks1 Tok_Main in 
  let toks3 = match_token toks2 Tok_LParen in
  let toks4 = match_token toks3 Tok_RParen in
  let toks5 = match_token toks4 Tok_LBrace in
  let (toks6,a) = parse_stmt toks5 in  
  if (toks6 = [Tok_RBrace; EOF]) then
    a
  else raise (InvalidInputException "main")
