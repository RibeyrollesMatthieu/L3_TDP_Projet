#use "lang.ml";;
#use "typing.ml";;
#use "exec.ml";;
#use "sets.ml";;

(* **************************************************************************************** *)
(* tests for typing functions *)

(*
(* Comparaison avec 2 meme types *)
let compar1 = BinOp(0, BCompar BCge, VarE(0, Var (Local, "n")), Const(0, IntV 1));;
(* comparaison avec 2 types differents *)
let compar2 = BinOp(0, BCompar BCge, VarE(0, Var (Local, "z")), Const(0, IntV 1));;


(* arith avec 2 int *)
let arithTrue = BinOp(0, BArith BAadd, VarE(0, Var (Local, "n")), Const(0, IntV 1));;
(* arith avec pas 2 int *)
let arithFaux = BinOp(0, BArith BAadd, VarE(0, Var (Local, "z")), Const(0, IntV 1));;



(* bound variable *)
let boundVar = VarE(0, Var(Local, "n"));;
(* unbound variable *)
let unboundVar = VarE(0, Var(Local, "o"));;


(* if (true) then 1 else 2 *)
let ifthenelse_good = IfThenElse(0, VarE(0, Var(Local, "z")), Const(0, IntV 3), Const(0, IntV 2));;
(* if (true) then 1 else 2 *)
let ifthenelse_false = IfThenElse(0, Const(0, BoolV false), Const(0, IntV 1), Const(0, IntV 2));;



let envTest = [("k", IntT); ("n", IntT); ("z", BoolT); ("r", IntT)];;

findInEnv "k" envTest;;
tp_expr envTest ifthenelse_good;;

*)
(* **************************************************************************************** *)



(* **************************************************************************************** *)
(* tests for evaluate fonctions *)



(* 
let state = [(Var(Local, "n"), IntV 4); (Var(Local, "r"), IntV 0); (Var(Local, "k"), IntV 0)];;
let test = Seq (Assign (IntT, Var (Local, "r"), Const (IntT, IntV 1)),While(BinOp (BoolT, BCompar BCgt, VarE (IntT, Var (Local, "n")),Const (IntT, IntV 0)),Seq(Assign (IntT, Var (Local, "r"),BinOp (IntT, BArith BAmul, VarE (IntT, Var (Local, "n")),VarE (IntT, Var (Local, "r")))),Assign (IntT, Var (Local, "n"),BinOp (IntT, BArith BAsub, VarE (IntT, Var (Local, "n")),Const (IntT, IntV 1))))));;
exec_stmt state test;;
*)

(* 
let test = IfThenElse(IntT, BinOp (BoolT, BCompar BCeq, VarE (IntT, Var (Local, "n")),BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "k")),Const (IntT, IntV 1))), Const(IntT, IntV 42), Const(IntT, IntV 24));;
eval_expr state test;; 
*)
(* **************************************************************************************** *)
























(* **************************************************************************************** *)
(*

Tests pour set_insert:

let a = [1; 2; 3];;
let b = set_insert 0 a;;	val b : int list = [1; 2; 3; 0]
set_insert 2 b;;			- : int list = [1; 2; 3; 0]



Tests pour set_union:
let a = [1; 2; 3];;
let b = [1; 4; 5; 2];;
set_union a b;;			- : int list = [1; 4; 5; 2; 3]


Tests pour set_inter:
set_inter a b;;		- : int list = [1; 2]


Tests pour set_diff:
set_diff a b;;		- : int list = [3]


Tests pour subset:
subset [1; 2; 3] [44; 7;8; 5; 6; 4; 7; 5; 2; 1];;	- : bool = false



Tests pour set_equal:
set_equal [1; 2; 3] [3; 2; 1];;		- : bool = true
set_equal [1; 2; 3] [3; 2; 4];;		- : bool = false





Tests pour fv:
fv test;;
- : var list = [Var (Local, "k"); Var (Local, "n")]



Tests pour defassign:
let test = Seq (Assign (IntT, Var (Local, "r"), Const (IntT, IntV 1)),While(BinOp (BoolT, BCompar BCgt, VarE (IntT, Var (Local, "n")),Const (IntT, IntV 0)),Seq(Assign (IntT, Var (Local, "r"),BinOp (IntT, BArith BAmul, VarE (IntT, Var (Local, "n")),VarE (IntT, Var (Local, "r")))),Assign (IntT, Var (Local, "n"),BinOp (IntT, BArith BAsub, VarE (IntT, Var (Local, "n")),Const (IntT, IntV 1))))));;
defassign [Var(Local, "n"); Var(Local, "r"); Var(Local, "k")] test;;


*)
(* **************************************************************************************** *)