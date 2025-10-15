(* Small Exercices *)

(* 6.1 : gcd without prime factors *)
let rec gcd a b =
  if a < b then gcd b a
  else
    let r = a mod b in
    if r = 0 then b else gcd b r ;;
