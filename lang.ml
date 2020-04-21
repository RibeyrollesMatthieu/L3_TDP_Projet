(* Definition of source language data structures *)



(* **************************************************************************************** *)
type tp = BoolT | IntT | VoidT;;	(* Types *)

type vname = string;; (* variable names *)
type fname = string;; (* function names *)
type binding = Local | Global;;
type var = Var of binding * vname;;
type vardecl = Vardecl of tp * vname;;	(* variable / parameter declaration *)
(* **************************************************************************************** *)



(* **************************************************************************************** *)
(* Binary operators *)
type barith = BAadd | BAsub | BAmul | BAdiv | BAmod;;	(* binary arithmetic operators *)
type blogic = BLand | BLor;;	(* binary logic operators *)
type bcompar = BCeq | BCge | BCgt | BCle | BClt | BCne;;	(* binary comparison operators *)


type binop =
	BArith of barith
|   BCompar of bcompar
|   BLogic of blogic;;
(* **************************************************************************************** *)


(* **************************************************************************************** *)
(* Value *)
type value =
	BoolV of bool
|   IntV of int
|   VoidV
|   UndefV;;
(* **************************************************************************************** *)



(* **************************************************************************************** *)
(* Expressions *)
(* The type parameter 'a is instantiated during type inference *)
type 'a expr = 
	Const of 'a * value	(* constant *)
|   VarE of 'a * var (* variable *)
|   BinOp of 'a * binop * ('a expr) * ('a expr) (* binary operation *)
|   IfThenElse of 'a * ('a expr) * ('a expr) * ('a expr);; (* if - then - else *)
(* For later:
  | CallE of 'a * fname * ('a expr list)
*)


(* The type parameter 'a is as for expressions *)
type 'a stmt =
	Skip
|   Assign of 'a * var * ('a expr)
|   Seq of ('a stmt) * ('a stmt)
|   Cond of ('a expr) * ('a stmt) * ('a stmt) 
|   While of ('a expr) * ('a stmt);;
(* For later:
  | CallC of fname * ('a expr list)
  | Return of ('a expr)
*)
(* **************************************************************************************** *)




(* **************************************************************************************** *)
(* Functions *)
(* extracts the type component of an expression *)
let tp_of_expr = function
	Const (t, _) -> t
|   VarE (t, _) -> t
|   BinOp (t, _, _, _) -> t
|   IfThenElse (t, _, _, _) -> t;;
	(*
  | CallE (t, _, _) -> t
	*)

let numeric_tp = function
	IntT -> true
|   t -> false;;

let tp_of_vardecl (Vardecl (t, _)) = t;;
let name_of_vardecl (Vardecl (_, vn)) = vn;;
(* ****************************************************************************************)