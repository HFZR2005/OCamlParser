
(* Terminals *)
type token = 
  | ADD
  | SUB
  | FACT
  | COS
  | NUM of float
  | LPAREN 
  | RPAREN 
  | POWER 
  | MUL ;; 

(* Defining nodes for the abstract syntax tree *)
type ast = 
  | Num of float
  | Add of ast * ast
  | Sub of ast * ast
  | Power of ast * ast 
  | Cos of ast 
  | Fact of ast ;; 

(* Utility function to print out token lists *)
let string_of_token = function
  | ADD -> "ADD"
  | SUB -> "SUB"
  | FACT -> "FACT"
  | COS -> "COS"
  | NUM n -> "NUM(" ^ string_of_float n ^ ")"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | POWER -> "POWER"
  | MUL -> "MUL"

(* Utility function to print out trees *)
let rec string_of_ast = function
  | Num n -> "Num(" ^ string_of_float n ^ ")"
  | Add (lhs, rhs) -> "Add(" ^ string_of_ast lhs ^ ", " ^ string_of_ast rhs ^ ")"
  | Sub (lhs, rhs) -> "Sub(" ^ string_of_ast lhs ^ ", " ^ string_of_ast rhs ^ ")"
  | Power (lhs, rhs) -> "Power(" ^ string_of_ast lhs ^ ", " ^ string_of_ast rhs ^ ")"
  | Cos exp -> "Cos(" ^ string_of_ast exp ^ ")"
  | Fact exp -> "Fact(" ^ string_of_ast exp ^ ")"



(* Parsing function *)

(* E -> T E' *)
let rec e toks =
  let (lhs, toks) = t toks in
  e' lhs toks

(* E' -> empty | (+ | -) T E' *)
and e' lhs = function
  | ADD :: toks ->
      let (rhs, toks) = t toks in
      e' (Add (lhs, rhs)) toks
  | SUB :: toks ->
      let (rhs, toks) = t toks in
      e' (Sub (lhs, rhs)) toks
  | toks -> (lhs, toks)

(* T -> F T' *)
and t toks =
  let (lhs, toks) = f toks in
  t' lhs toks

(* T' -> empty | ^ F T' *)
and t' lhs = function
  | POWER :: toks ->
      let (rhs, toks) = f toks in
      t' (Power (lhs, rhs)) toks
  | toks -> (lhs, toks)

(* F -> COS F | F ! | ( expr ) | Num ! | Num *)
and f = function
  | COS :: LPAREN :: toks ->
      let (exp, toks) = f (LPAREN :: toks) in
      (Cos exp, toks)
  | LPAREN :: toks ->
      let (exp, toks) = e toks in
      (match toks with
      | RPAREN :: FACT :: toks -> (Fact exp, toks)
      | RPAREN :: toks -> (exp, toks)
      | _ -> failwith "Unmatched parenthesis")
  | NUM n :: FACT :: toks -> (Fact (Num n), toks)
  | NUM n :: toks -> (Num n, toks)
  | _ -> failwith "Invalid Token"

let print_tokens toks =
  let rec aux = function
    | [] -> print_endline ""
    | tok :: rest ->
        print_string (string_of_token tok ^ " ");
        aux rest
  in aux toks

let print_ast ast = print_endline (string_of_ast ast) ;; 
let print_res ast res = print_tokens res; print_ast ast ;; 

(* Think this parser works, returns empty list of tokens if valid, now need to construct a tree from it *)

(* Want to create AST from parsing -> base cases at factor / num level, at each level, get ast and tokens -> match on token and continue developing ASTs as accumulators *)



(* Now want to calculate values after parsing, write a function that checks if something is correct -> if correct return its ast and parse tree, otherwise raise an exception *)

exception SyntaxError;;

let check tok_list = 
  let ast, toks = e tok_list in 
  match toks with
  | [] -> ast
  | _ -> raise SyntaxError ;; 



(* Rubbish factorial approximation *)
let rec fact n = if n < 1. then 1. else n *. fact (n -. 1.)

(* Function which takes an AST and evaluates it *)
let rec eval ast = 
  match ast with
  | Num n -> n
  | Fact ast -> fact (eval ast)
  | Cos ast -> Float.cos (eval ast)
  | Add (t1, t2) -> (eval t1) +. (eval t2)
  | Sub (t1, t2) -> (eval t1) -. (eval t2)
  | Power (t1, t2) -> Float.pow (eval t1) (eval t2) ;;

(* Function which takes a token list and evaluates it *)
let eval tok_list = 
  let ast = check tok_list in 
  print_res ast []; 
  eval ast ;; 


let tok_list = [NUM 3.; ADD; COS; LPAREN; NUM 3.; RPAREN]
let thing = eval tok_list;; 


(* Current issues -> can't chain factorials without parenthesis but can chain COS without parenthesis. Current implementation only accepts factorials after Nums or RParens, should allow them after FACTS aswell 

Possible fix -> upon reading a factorial, look at the tree built so far, Put it in a factorial -> if there isn't a tree, raise an error

Convert factorial into a prefix operator -> place it before the factor each time, and then handle it the same way as COS

Once recieving token list, reformat it so that it parses correctly -> so hacky

so COS NUM 3. -> COS LPAREN NUM 3. RPAREN

Function which takes an expression, wraps all the cos terms in brackets -> currently enforcing that all cos terms have arguments supplied in brackets. 
   *)

let rec reformat tok_list =
  let rec inner tok_list num = 
    match tok_list with 
    | [] -> (match num with
      | 0 -> []
      | k -> RPAREN :: (inner tok_list (num - 1)))
    | COS :: toks -> LPAREN :: COS :: (inner toks (num + 1))
    | LPAREN :: toks -> LPAREN :: (inner toks (num + 1))
    | RPAREN :: toks -> (match num with
        | 1 -> RPAREN :: RPAREN :: (inner toks num)
        | k -> RPAREN :: (inner toks (num - 1)))
    | tok::toks -> tok :: (inner toks num)  
  in inner tok_list 0 ;; 

reformat tok_list ;; 
print_endline (string_of_float thing) ;; 
print_tokens (reformat tok_list) ;;


(* Helper functions for the lexer *)

let is_digit c =
  '0' <= c && c <= '9' ;; 

let is_letter c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ;;


