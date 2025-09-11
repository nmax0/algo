(* level 1 *)

let months = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] ;;

let tmrw d m y =
  if m < 1 || m > 12 then failwith "not a valid month";
  let d_max = List.nth months (m-1) in
  if d > d_max then failwith "not a valid day";
  if (d+1) > d_max then
    if (m+1) > 12 then
      (1, 1, y+1)
    else (1, m+1, y)
  else (d+1, m, y) ;;
