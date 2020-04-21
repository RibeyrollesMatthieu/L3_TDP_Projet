(* #use "lang.ml";; *)

(* Environments *)

type environment =  (vname * tp) list;;

(* ************************************************************ *)
(***** Auxiliary functions *****)

(* Return the type of the binOp *)
let typeOfBinOp = function
	BArith _ -> IntT
| 	_	-> BoolT;;

(* check if the binOp is correctly used *)
let canBinOp op t1 t2 = match op with
	BArith _ -> (t1 = t2) && (t1 = IntT)
| 	_	-> (t1 = t2);;
(* ************************************************************ *)


(* ************************************************************ *)
(* return the expression with all the rights types: a tp expr *)
let rec tp_expr (env: (vname * tp) list) = function
	IfThenElse(_, cond, e_then, e_else)	-> 
		let tpE_then = (tp_expr env e_then) and tpE_else = (tp_expr env e_else) and tpE_cond = (tp_expr env cond)
		in 
		if ( ((tp_of_expr tpE_then) = tp_of_expr tpE_else) && ((tp_of_expr tpE_cond) = BoolT))
			then IfThenElse(tp_of_expr tpE_then, tpE_cond, tpE_then, tpE_else)
			else failwith "wrong usage of ifThenElse"

| 	BinOp(_, op, e1, e2)	->	
		let tpE1 = (tp_expr env e1) and tpE2 = (tp_expr env e2) 
		in
		if (canBinOp op (tp_of_expr tpE1) (tp_of_expr tpE2))
			then BinOp((typeOfBinOp op), op, tpE1, tpE2) 
			else failwith "wrong usage of binOp"

| 	Const(_, IntV v)	-> 	Const(IntT, IntV v)
| 	Const(_, BoolV v)	->  Const(BoolT, BoolV v)
| 	Const(_, _)	-> failwith "Not a valid constant"

| 	VarE(_, (Var(_, lettre) as variable))	-> (try VarE((List.assoc lettre env), variable) with _ -> failwith "unbound variable");; 
(* ************************************************************ *)



(* ************************************************************ *)
(* enforcing that type of a CallC fct to be VoidT is not strics necessary *)
(* return the statement with all the right types: tp stmt *)
let rec tp_stmt env = function
 	Cond(a_expr, stmt1, stmt2) 	->	
		let tp_a_expr = tp_expr env  a_expr

		in 
		if (tp_of_expr tp_a_expr = BoolT)
		then Cond(tp_a_expr, tp_stmt env stmt1, tp_stmt env stmt2) 
		else failwith "cond must be a boolean"


| 	Seq(stmt1, stmt2)	-> Seq(tp_stmt env stmt1, tp_stmt env stmt2)
| 	While(a_expr, a_stmt) 	->	let tp_a_expr = tp_expr env a_expr 
								in 
								if (tp_of_expr tp_a_expr = BoolT)
								then While(tp_a_expr, tp_stmt env a_stmt)
								else failwith "while cond must be a boolean"	
| 	Assign(_, Var(_, lettre), valeur)	-> let 	tp_valeur = tp_expr env valeur in 
												Assign(tp_of_expr tp_valeur, Var(Local, lettre), tp_valeur)
|	Skip	->	Skip;;	
(* ************************************************************ *)