let dmax x = (* non leap years *)
  match x with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> 28
  | _ -> failwith "not a valid month";;

let int_of_month x =
  match x with
  | "January" -> 1
  | "February" -> 2
  | "March" -> 3
  | "April" -> 4
  | "May" -> 5
  | "June" -> 6
  | "July" -> 7
  | "August" -> 8
  | "September" -> 9
  | "October" -> 10
  | "November" -> 11
  | "December" -> 12
  | _ -> failwith "not a valid month";;

let month_of_int x =
  match x with
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> failwith "not a valid month";;

(* level 1 *) 

let tmrw_int d m y =
  let dmax = dmax m in
  if d > dmax then failwith "not a valid day";
  if (d+1) > dmax then
    if (m+1) > 12 then
      (1, 1, y+1)
    else (1, m+1, y)
  else (d+1, m, y) ;;

(* level 2 *)

let tmrw_str s =
  if String.length s = 10 then (* format : "00/00/0000" *)
    let d1 = int_of_string (String.sub s 0 2) in (* unpacked day val *)
    let m1 = int_of_string (String.sub s 3 2) in (* unpacked month val *)
    let y1 = int_of_string (String.sub s 6 4) in (* unpacked year val *)
    let d2, m2, y2 = tmrw_int d1 m1 y1 in (* compute tmrw's date *)
    let d3 = if d2 < 10 then "0" ^ string_of_int d2 else string_of_int d2 in (* 9 -> 09 *)
    let m3 = if m2 < 10 then "0" ^ string_of_int m2 else string_of_int m2 in (* 9 -> 09 *)
    d3 ^ "/" ^ m3 ^ "/" ^ string_of_int y2 (* concatenation *)
  else (* format : "Month day, year" *)
    let len = String.length s in
    let month_len = len-9 in
    let m1 = int_of_month (String.sub s 0 (month_len)) in (* unpacked month val *)
    let d1 = int_of_string (String.sub s (month_len+1) 2) in (* unpacked day val *)
    let y1 = int_of_string (String.sub s (month_len+5) 4) in (* unpacked year val *)
    let d2, m2, y2 = tmrw_int d1 m1 y1 in (* comute tmrw's date *)
    let d3 = if d2 < 10 then "0" ^ string_of_int d2 else string_of_int d2 in (* 9 -> 09 *)
    let m3 = month_of_int m2 in (* 1 -> January *)
    m3 ^ " " ^ d3 ^ ", " ^ string_of_int y2 ;; (* concatenation *)

(*
tmrw_int 25 12 2025 ;;
tmrw_str "25/12/2025" ;;
tmrw_str "December 25, 2025" ;;
*)
