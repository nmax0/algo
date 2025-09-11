let perf_sq x =
  let a = sqrt (float_of_int x) in
  let b = int_of_float a in
  a = float_of_int b ;;

let sum x = 
  let x1 = x / 100 in
  let x2 = x / 10 mod 10 in
  let x3 = x mod 10 in 
  
  let max = max (max x1 x2) x3 in
  let min = min (min x1 x2) x3 in
  let mid = x1 + x2 + x3 - max - min in
  
  perf_sq (max * max + mid * mid) ;;

let main x =
  if x >= 1000 || x < 100 then invalid_arg "3 digit int is needed" else
    perf_sq x && sum x ;;

(* code = 100, 400, 900 *)
