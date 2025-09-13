let months = ("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") ;;

(* helper functions *)
let add_zero x =   if x < 10 then "0" ^ string_of_int x else string_of_int x ;; (* helper format : 9 -> 09 *)
let normalize s = String.capitalize_ascii (String.lowercase_ascii s) ;; (* helper format : normalize str *)

(* conversion functions *)
let m2i str = (* month to int : "December" -> 12 *)
  let s = normalize str in
  match months with (jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) -> (* link jan to "January" *)
    match s with
    | s when s = jan -> 1 | s when s = feb -> 2 | s when s = mar -> 3
    | s when s = apr -> 4 | s when s = may -> 5 | s when s = jun -> 6
    | s when s = jul -> 7 | s when s = aug -> 8 | s when s = sep -> 9
    | s when s = oct -> 10 | s when s = nov -> 11 | s when s = dec -> 12
    | _ -> failwith "not a valid month" ;;
let i2m int = (* int to month : 12 -> "December" *)
  match months with (jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) -> (* link feb to "February" *)
    match int with
    | 1 -> jan | 2 -> feb | 3 -> mar | 4 -> apr | 5 -> may | 6 -> jun 
    | 7 -> jul | 8 -> aug | 9 -> sep | 10 -> oct | 11 -> nov | 12 -> dec
    | _ -> failwith "not a valid month" ;;
let dim x = (* days in month *)
  match x with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> 28
  | _ -> failwith "not a valid month";;


let tmrw_int d m y = (* level 1 : int *)  (* format : D MM YYYY *)
  let dim = days_in_month m in
  if d < 1 || d > dim then failwith "not a valid day"; 
  if d < dim then (d+1, m, y)
  else if m < 12 then (1, m+1, y)
  else (1, 1, y+1) ;; (* happy new year *)

let tmrw_str s = (* level 2 : str *)
  if String.length s = 10 then (* format : "DD/MM/YYYY" *)
    (* if String.get s 2 <> "/" || String.get s 5 <> "/" then failwith "incorrect date"; *)
    let d1 = int_of_string (String.sub s 0 2) in (* unpacked day val *)
    let m1 = int_of_string (String.sub s 3 2) in (* unpacked month val *)
    let y1 = int_of_string (String.sub s 6 4) in (* unpacked year val *)
    let d2, m2, y2 = tmrw_int d1 m1 y1 in (* compute tmrw's date *)
    add_zero d2 ^ "/" ^ add_zero m2 ^ "/" ^ string_of_int y2 (* concatenation *)
  else (* format : "Month DD, YYYY" *)
    let month_len = (String.length s)-9 in
    (* if String.get s month_len <> ' ' || String.get s (month_len+3) <> ',' || String.get s (month_len+4) <> ' ' then failwith "incorrect date" *)
    let m1 = m2i (String.sub s 0 (month_len)) in (* unpacked month val *)
    let d1 = int_of_string (String.sub s (month_len+1) 2) in (* unpacked day val *)
    let y1 = int_of_string (String.sub s (month_len+5) 4) in (* unpacked year val *)
    let d2, m2, y2 = tmrw_int d1 m1 y1 in (* comute tmrw's date *)
    i2m m2 ^ " " ^ add_zero d2 ^ ", " ^ string_of_int y2 ;; (* concatenation *)

(* 
tmrw_int 5 12 2025 ;;
tmrw_str "05/12/2025" ;;
tmrw_str "December 05, 2025" ;;
*)
