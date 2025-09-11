let perf_sq x =
  let n = int_of_float (sqrt (float_of_int x)) in
  n * n = x ;;

let check x = 
  if x < 100 || x > 999 then invalid_arg "3 digit int required";
  
  if not (perf_sq x) then false else
  
    let x1 = x / 100 in
    let x2 = x / 10 mod 10 in
    let x3 = x mod 10 in 
  
    let max = max (max x1 x2) x3 in
    let min = min (min x1 x2) x3 in
    let mid = x1 + x2 + x3 - max - min in
  
    perf_sq (max * max + mid * mid) ;;

(* code = 100, 400, 900 *)
