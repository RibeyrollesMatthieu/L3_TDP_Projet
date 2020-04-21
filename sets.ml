(* set insert : ’a -> ’a list -> ’a list *)
let rec set_insert e = function
	(a::q) 	->	if (e <> a) then a::set_insert e q else (a::q)
| 	[] 		->	[e];;

(* val set_union : 'a list -> 'a list -> 'a list = <fun> *)
let rec set_union e1 e2 = match e1 with
	(a::q)	->	set_union q (set_insert a e2)
| 	[]		->	e2;;

(* val set_inter : 'a list -> 'a list -> 'a list = <fun> *)
let rec set_inter e1 e2 = match e1 with
	(a::q)	->	if (List.mem a e2) then a::(set_inter q e2) else set_inter q e2 
| 	[]		->	[];;

(* val set_diff : 'a list -> 'a list -> 'a list = <fun> *)
let rec set_diff e1 e2 = match e1 with
	(a::q)	->	if (List.mem a e2) then set_diff q e2 else a::(set_diff q e2)
| 	[]		->	[];;

(* val subset : 'a list -> 'a list -> bool = <fun> *)
let subset e1 e2 = set_diff e1 e2 = [];;

(* val set_equal : 'a list -> 'a list -> bool = <fun> *)
let set_equal e1 e2 = subset e1 e2 && subset e2 e1;; 