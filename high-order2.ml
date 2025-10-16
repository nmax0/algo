(* 2. Functions with lists *)

(* 2.1 : map f l (f[a1;a2] -> [f(a1);f(a2)]) *)
let rec map f = function
  | [] -> []
  | e::l -> (f e)::map f l ;;
(* debug
  map (fun x -> x*x) [1;4;6] ;;
*)

(* 2.2 : check p for all elem in l *)
let rec for_all p = function
  | [] -> true
  | e::l -> if p e then for_all p l else false ;;
(* debug
  for_all (fun x -> x>0) [1;3;5] ;;
  for_all (fun x -> x>0) [1;3;-5] ;;
*)

(* 2.3 : check if at least one elem in l check p *)
let rec for_all p = function
  | [] -> false
  | e::l -> if p e then true else for_all p l ;;
(* debug
  for_all (fun x -> x>0) [-3;-5] ;;
  for_all (fun x -> x>0) [-2;3] ;;
*)
