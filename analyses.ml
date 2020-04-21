(* #use lang.ml *)
(* #use sets.ml *)

(* val fv : 'a expr -> var list = <fun> *)
let rec fv = function
	BinOp(_, _, e1, e2)					-> set_union (fv e1) (fv e2) 
|   IfThenElse(_, e_if, e_then, e_else) -> set_union (set_union (fv e_if) (fv e_then)) (fv e_else)
|   VarE(_, v) 							-> [v]
|	Const(_, _) 						-> [];;

(* val defassign : var list -> 'a stmt -> var list = <fun> *)
let rec defassign vars = function
 	While(expr, stmt)			->	if (subset (fv expr) vars) 
		 							then vars
		 							else failwith (errorIn expr)

| 	Cond(expr, stmt1, stmt2)	->	if (subset (fv expr) vars) 
									then set_inter (defassign vars stmt1) (defassign vars stmt2)  
									else failwith (errorIn expr)

| 	Seq(stmt1, stmt2)			->  (defassign (defassign vars stmt1) stmt2) 
	
| 	Assign(_, var, expr)		-> 	if (subset (fv expr) vars) 
									then set_union [var] vars
									else failwith (errorIn expr)

|	Skip						->	vars;;					

(* val errorIn : 'a expr -> string = <fun> *)
let rec errorIn = function
 	IfThenElse(_, e1, e2, e3)	-> (errorIn e1) ^ (errorIn e2) ^ (errorIn e3)
| 	BinOp(_, _, e1, e2)			-> (errorIn e1) ^ (errorIn e2)
| 	VarE(_, Var(_, name))		-> "Variable " ^ name ^ " has not been initialized. "
| 	Const(_, _)					-> "";; 

(* val live : 'a stmt -> var list -> var list = <fun *)
let rec live stm vars = match stm with
	While(expr, stmt)			->	set_union (fv expr) (set_union (live stmt vars) vars)
| 	Cond(expr, stmt1, stmt2)	->	set_union (fv expr) (set_union (live stmt1 vars) (live stmt2 vars))
| 	Seq(stmt1, stmt2)			->  live stmt2 (live stmt1 vars)
| 	Assign(_, var, expr)		-> 	set_union (fv expr) (set_diff vars [var])
|	Skip						->	vars;;

(* val optim_stmt : 'a stmt -> var list -> 'a stmt = <fun> *)
let rec optim_stmt stm vvs = match stm with 
	While(expr, stmt)			->	While(expr, optim_stmt stmt vvs)
| 	Cond(expr, stmt1, stmt2)	->	Cond(expr, (optim_stmt stmt1 vvs), (optim_stmt stmt2 vvs))
| 	Seq(stmt1, stmt2)			->  Seq( (optim_stmt stmt1 vvs), (optim_stmt stmt2 vvs))
| 	Assign(t, var, expr)		-> 	if (List.mem var vvs) 
									then Skip
									else Assign(t, var, expr)
|	Skip						->	Skip;;

(* val clean_seq : 'a stmt -> 'a stmt = <fun> *)
let clean_seq (Seq(stmt1, stmt2)) = if (stmt1 = Skip)
									then
										if (stmt2 = Skip)
										then Skip
										else stmt2
									else
										if (stmt2 = Skip)
										then stmt1
										else Seq(stmt1, stmt2);; 

(* val clean_stmt : 'a stmt -> 'a stmt = <fun> *)
let rec clean_stmt = function
	While(expr, stmt)			->	While(expr, clean_stmt stmt)
| 	Cond(expr, stmt1, stmt2)	->	Cond(expr, clean_stmt stmt1, clean_stmt stmt2)
| 	Seq(stmt1, stmt2)			-> 	clean_seq (Seq(clean_stmt stmt1, clean_stmt stmt2))
| 	Assign(t, var, expr)		-> 	Assign(t, var, expr)
|	Skip						->	Skip;;

(* val integration : 'a stmt -> var list -> 'a stmt = <fun> *)
let integration stmt vars = clean_stmt (optim_stmt stmt ( live stmt vars ));;