(* 3.7 : price according to weight and delivery type *)
let rate w t =
  let ese w =
    match w with
    | w when w < 0 -> invalid_arg "weight cant be a negative number"
    | w when w < 500 -> (3.4, 4.6, 9.1)
    | w when w < 1000 -> (4.6, 5.9, 11.)
    | w when w < 2000 -> (5.1, 6.5, 13.5)
    | w when w < 3000 -> (6.9, 7.2, 14.2)
    | _ -> failwith "too heavy" in
  match ese w with (ec, st, ex) -> 
    match t with
    | "eco" -> ec
    | "standard" -> st
    | "express" -> ex
    | _ -> failwith "unknow code" ;;

  (* 3.10 : time difference between two longitude coordinates *)
  let time_dif (a1, b1, c1, d1) (a2, b2, c2, d2) =
  let s1 = (a1*240 + b1*4 + c1/15) and s2 = (a2*240 + b2*4 + c2/15) in (* Δt between each coordinates and the meridian line in s *)
  let s1 = if d1 = 'W' then -s1 else s1 and s2 = if d2 = 'W' then -s2 else s2 in (* add signs for comparisons *)
  if s1 > s2 then
    let dif = s1 - s2 in (* actual time difference in seconds between longitude 1 and 2 *)
    let dh, dm, ds = dif / 3600, dif mod 3600 / 60, dif mod 60 in (* conversion in hours, minutes and seconds *)
    "earlier : " ^ string_of_int dh ^ "h " ^ string_of_int dm ^ "m " ^ string_of_int ds ^ "s" (* concatenation *)
  else (* coordinate format : a° b' c" *)
    let dif = s2 - s1 in
    let dh, dm, ds = dif / 3600, dif mod 3600 / 60, dif mod 60 in
    "later : " ^ string_of_int dh ^ "h " ^ string_of_int dm ^ "m " ^ string_of_int ds ^ "s" ;;
