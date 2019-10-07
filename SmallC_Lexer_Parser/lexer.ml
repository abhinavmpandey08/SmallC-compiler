open SmallCTypes
open Str

let tokenize input = 
  let rec tok pos s =
    if pos >= String.length s then [EOF]
    else
      if (string_match (regexp "(") s pos) then
        (Tok_LParen)::(tok (pos+1) s)
      else if (string_match (regexp ")") s pos) then
        (Tok_RParen)::(tok (pos+1) s)
      else if (string_match (regexp "{") s pos) then
        (Tok_LBrace)::(tok (pos+1) s)
      else if (string_match (regexp "}") s pos) then
        (Tok_RBrace)::(tok (pos+1) s)
      else if (string_match (regexp "==") s pos) then
        (Tok_Equal)::(tok (pos+2) s)
      else if (string_match (regexp "!=") s pos) then
        (Tok_NotEqual)::(tok (pos+2) s)
      else if (string_match (regexp "=") s pos) then
        (Tok_Assign)::(tok (pos+1) s)
      else if (string_match (regexp ">") s pos) then
        (Tok_Greater)::(tok (pos+1) s)
      else if (string_match (regexp "<") s pos) then
        (Tok_Less)::(tok (pos+1) s)
      else if (string_match (regexp ">=") s pos) then
        (Tok_GreaterEqual)::(tok (pos+2) s)
      else if (string_match (regexp "<=") s pos) then
        (Tok_LessEqual)::(tok (pos+2) s)
      else if (string_match (regexp "||") s pos) then
        (Tok_Or)::(tok (pos+2) s)
      else if (string_match (regexp "&&") s pos) then
        (Tok_And)::(tok (pos+2) s)
      else if (string_match (regexp "!") s pos) then
        (Tok_Not)::(tok (pos+1) s)
      else if (string_match (regexp ";") s pos) then
        (Tok_Semi)::(tok (pos+1) s)
      else if (string_match (regexp "int") s pos) then
        (Tok_Int_Type)::(tok (pos+3) s)
      else if (string_match (regexp "bool") s pos) then
        (Tok_Bool_Type)::(tok (pos+4) s)
      else if (string_match (regexp "printf") s pos) then
        (Tok_Print)::(tok (pos+6) s)
      else if (string_match (regexp "main") s pos) then
        (Tok_Main)::(tok (pos+4) s)
      else if (string_match (regexp "if") s pos) then
        (Tok_If)::(tok (pos+2) s)
      else if (string_match (regexp "else") s pos) then
        (Tok_Else)::(tok (pos+4) s)
      else if (string_match (regexp "while") s pos) then
        (Tok_While)::(tok (pos+5) s)
      else if (string_match (regexp "+") s pos) then
        (Tok_Add)::(tok (pos+1) s)
      else if (string_match (regexp "-?[0-9]+") s pos) then
        let token = matched_string s in
        (Tok_Int (int_of_string token))::(tok (pos+(String.length token)) s)
      else if (string_match (regexp "-") s pos) then
        (Tok_Sub)::(tok (pos+1) s)
      else if (string_match (regexp "*") s pos) then
        (Tok_Mult)::(tok (pos+1) s)
      else if (string_match (regexp "/") s pos) then
        (Tok_Div)::(tok (pos+1) s)
      else if (string_match (regexp "\\^") s pos) then
        (Tok_Pow)::(tok (pos+1) s)
      else if(string_match (regexp "true\\|false") s pos) then
	let token = (matched_string s) in
	(Tok_Bool (token = "true"))::(tok (pos+(String.length token)) s)
      else if (string_match (regexp "[a-zA-Z][a-zA-Z0-9]*") s pos) then
        let token = matched_string s in
        (Tok_ID token)::(tok (pos+(String.length token)) s)
      else if(string_match (regexp "[ \t\n]") s pos) then 
	let token = matched_string s in
        (tok (pos+(String.length token)) s)
      else
	raise (InvalidInputException "TokenizeFailure")
  in
  tok 0 input
;;
 
