let surf a =
if a < 0 then invalid_arg "negative surface" else
let b = a - 5000 in
let c = b * 1000 + b in
let d = c / 7 - 7 * (c mod 7) in
let e = d / 11 - 11 * (d mod 11) in
let f = e / 13 - 13 * (e mod 13) in
f = 555 ;;

(* code = 5555 *)
