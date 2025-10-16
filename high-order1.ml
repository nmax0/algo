(* higher order functions ? *)

(* 1.2.1 : sum [0->n] i *)
let rec sum = function
  | n when n < 0 -> invalid_arg ""
  | 0 -> 0
  | n -> n + sum (n-1) ;;
  
(* 1.2.2 : sum [0->n] i² *)
let rec sq_sum = function
  | n when n < 0 -> invalid_arg ""
  | 0 -> 0
  | n -> n*n+sq_sum (n-1) ;;
  
(* 1.2.3 : sum [0->n] f(i) *)
let rec fsum f = function
  | n when n < 0 -> invalid_arg ""
  | 0 -> 0
  | n -> f n + fsum f (n-1) ;;

(* 1.2.2 (v2) *)
let square = function x -> x*x ;;
let sq_sum = function n -> fsum square n ;;

(* 1.3.1 : loop until p(f(x)) is true *)
let rec loop p f x = if p x then x else loop p f (f x) ;;

(* 1.3.2 : returns first power x >= n *)
let find_power x n = loop (function x -> x >= n) (function y -> y * x) x ;;
