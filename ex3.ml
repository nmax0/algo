let mir x = 
let x1 = x / 1000 in
let x2 = x / 100 mod 10 in
let x3 = x / 10 mod 10 in
let x4 = x mod 10 in
x4 * 1000 + x3 * 100 + x2 * 10 + x1

let myst x =
let sqf = sqrt (float_of_int x) in
let sqi = float_of_int (int_of_float sqf) in
if sqf = sqi then "1" else "0" ;;

let crack x =
if x < 0 || x >= 10000 || x < 1000 then invalid_arg "inv arg" else 
let x1 = x / 1000 in
let x2 = x / 100 mod 10 in
let x3 = x / 10 mod 10 in
let x4 = x mod 10 in
if x1 mod 2 = 0 && x2 mod 2 = 0 && x3 mod 2 = 0 && x4 mod 2 = 1 then
if x1+x2+x3+x4 = 9 then
if float_of_int (x / 25) = (float_of_int x) /. 25. then
let y = x + mir x in
if y = mir y then
if myst x = "1" then
true
else false
else false
else false
else false
else false ;;

(* code = 2025 *)
