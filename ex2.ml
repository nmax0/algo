let top x =
let x1 = x / 100 in
let x2 = x / 10 mod 10 in
let x3 = x mod 10 in

let max = if x1 > x2 && x1 > x3 then x1 else if x2 > x3 then x2 else x3 in
let min = if x1 < x2 && x1 < x3 then x1 else if x2 < x3 then x2 else x3 in
let mid = if x1 <> min && x1 <> max then x1 else if x2 <> min && x2 <> max then x2 else x3 in

let sum = (mid * mid + max * max) in
let m = myst sum in
if m = "1" then "1" else "0" ;;

let myst x =
let sqf = sqrt (float_of_int x) in
let sqi = float_of_int (int_of_float sqf) in
if sqf = sqi then "1" else "0" ;;

let main x =
if (float_of_int x /. 1000. > 1.) || (float_of_int x /. 10. < 10.) then invalid_arg "3 digit num is needed" 
else (myst x, top x) ;;

main 484 ;;
