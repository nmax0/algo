(* Small Exercices *)

(* 6.1 : gcd *)
(* without prime factors *)
let rec gcd a b =
  let r = a mod b in
  if r = 0 then b else gcd b r ;;
(* with prime factors *)
let rec product l1 =
  match l1 with
  | e::l -> e*product l
  | _ -> 1 ;;
let rec common l1 l2 =
  match l1, l2 with 
  | e1::l1, e2::l2 when e1 = e2 -> e1::common l1 l2
  | e1::l1, e2::l2 when e1 > e2 -> common (e1::l1) l2
  | e1::l1, l2 -> common l1 l2
  | _ -> [] ;;
let prime_factor x =
  let rec aux x d =
    if x = 1 then [] 
    else if x mod d = 0 then d::aux (x/d) d
    else aux x (d+1) in
  aux x 2 ;;
let rec gcd a b =
  product (common (prime_factor a) (prime_factor b)) ;;
