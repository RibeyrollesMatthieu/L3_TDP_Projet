(* #use "lang.ml";; *)

(* ************************************************************ *)
(* ****  Operational Semantics                             **** *)
(* ************************************************************ *)

type state = (var * value) list;;

(* **************************************************************************************** *)
(* Functions used to convert binary operators to elemental binary expressions *)
let eval_barith = function
	BAadd 	-> ( + )
| 	BAsub 	-> ( - )
| 	BAmul 	-> ( * )
| 	BAdiv	-> ( / )
| 	BAmod	-> ( mod );;

let eval_blogic  = function
	BLand	-> 	( && )
| 	BLor	->	( || );;

let eval_bcompar = function
	BCeq	-> 	( = ) 
| 	BCge 	->	( >= )
| 	BCgt 	->	( > )
| 	BCle 	->	( <= )
| 	BClt 	->	( < )
| 	BCne	->	( <> );;

(* exception GivenBinaryOperatorExpressionIsNotCorrectlyUsed;;  to replace failwith *)

(* Function using above functions depending of the given binary operator. Return a Value *)
let eval_binop = function 
	(BArith ba), (IntV v1), (IntV v2)	-> IntV ((eval_barith ba) v1 v2)
| 	(BCompar bc), (IntV v1), (IntV v2)	-> BoolV ((eval_bcompar bc) v1 v2)
| 	(BCompar bc), (BoolV v1), (BoolV v2)	-> BoolV ((eval_bcompar bc) v1 v2)
| 	(BLogic bl), (BoolV v1), (BoolV v2)	-> BoolV ((eval_blogic bl) v1 v2)
| 	_ -> failwith "wrong usage of binary operators";;
(* **************************************************************************************** *)


(* **************************************************************************************** *)

(* exception IfExpressionsNeedBooleanCondition;; 	to replace failwith*)

(* Functions used to evaluate given 'a expr *)
let rec eval_expr (st: (var * value) list) = function
 	IfThenElse(_, e_cond, e_then, e_else)	->
 		let cond = eval_expr st e_cond
 		in
 		if ( cond = BoolV true )
 			then eval_expr st e_then
 			else if ( cond = BoolV false )
 				then eval_expr st e_else
 				else failwith "if expression needs a boolean condition"
| 	BinOp(_, op, e1, e2)	->	eval_binop (op, eval_expr st e1, eval_expr st e2) 
|	Const(_, value)	-> value 
| 	VarE(_, variable)	-> 	(try (List.assoc variable st) with _ -> failwith "variable not found");;
(* **************************************************************************************** *)



(* **************************************************************************************** *)

(* exception UndefinedVariableName;; to replace failwith*)

(* Fonction used to update a variable in the given state. If it does not exists, raise an exception. *)
let rec upd_var variable newValue = function
	((((currentVar, currentValue) as a)::q):(var * value) list)	->	if (variable = currentVar) then (currentVar, newValue)::q else a::(upd_var variable newValue q)
| 	_	-> failwith "undefined variable";;	(* on suppose que ça crée la variable. A voir si le vardecl s'en charge ou pas *)
(* **************************************************************************************** *)



(* **************************************************************************************** *)
(* Functions used to evaluate given 'a stmt *)
let rec exec_stmt st = function
 	Assign(_, variable, a_expr)	-> upd_var variable (eval_expr st a_expr) st	
| 	Seq(stmt1, stmt2)	->	exec_stmt (exec_stmt st stmt1) stmt2
| 	Cond(a_expr, stmt1, stmt2) 	->  if (eval_expr st a_expr = BoolV true) then exec_stmt st stmt1 else exec_stmt st stmt2
| 	While(a_expr, a_stmt) 	-> if (eval_expr st a_expr = BoolV true) then exec_stmt (exec_stmt st a_stmt) (While(a_expr, a_stmt)) else st	
|	Skip	->	st;;
(* ****************************************************************************************)