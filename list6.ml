(* Small Exercices *)

(* 6.1 : gcd *)
(* without prime factors *)
let rec gcd a b =
  let r = a mod b in
  if r = 0 then b else gcd b r ;;
(* with prime factors *)
(* x.x *)
