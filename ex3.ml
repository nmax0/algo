let mirror x = 
  let x1 = x / 1000 in
  let x2 = x / 100 mod 10 in
  let x3 = x / 10 mod 10 in
  let x4 = x / 1 mod 10 in
  x4 * 1000 + x3 * 100 + x2 * 10 + x1

let perf_sq x =
  let n = int_of_float (sqrt (float_of_int x)) in
  n * n = x ;;

let code x =
  if x < 1000 || x > 9999 then invalid_arg "3 digit int required" else
    let x1 = x / 1000 in
    let x2 = x / 100 mod 10 in
    let x3 = x / 10 mod 10 in
    let x4 = x / 1 mod 10 in
    if x1 mod 2 = 0 && x2 mod 2 = 0 && x3 mod 2 = 0 && x4 mod 2 = 1
       && x1 + x2 + x3 + x4 = 9
       && float_of_int (x / 25) = (float_of_int x) /. 25.
       && (x + mirror x) = mirror (x + mirror x)
       && perf_sq x then true else false ;;

(* code = 2025 *)
