let months = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] ;; (* amount of days per months (non leap year) *)

(* level 1 *) 

let tmrw1 d m y =
  if m < 1 || m > 12 then failwith "not a valid month";
  let d_max = List.nth months (m-1) in
  if d > d_max then failwith "not a valid day";
  if (d+1) > d_max then
    if (m+1) > 12 then
      (1, 1, y+1)
    else (1, m+1, y)
  else (d+1, m, y) ;;

(* level 2 *)

let tmrw2 s =
  if String.length s = 10 then
    let d1 = int_of_string (String.sub s 0 2) in (* unpacked day val *)
    let m1 = int_of_string (String.sub s 3 2) in (* unpacked month val *)
    let y1 = int_of_string (String.sub s 6 4) in (* unpacked year val *)
    let d2, m2, y2 = tmrw1 d1 m1 y1 in (* tmrw's date *)
    let d3 = if d2 < 10 then "0" ^ string_of_int d2 else string_of_int d2 in (* 9 -> 09 *)
    let m3 = if m2 < 10 then "0" ^ string_of_int m2 else string_of_int m2 in (* 9 -> 09 *)
    d3 ^ "/" ^ m3 ^ "/" ^ string_of_int y2 (* concatenation *)
  else "ciao" ;;
