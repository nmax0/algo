let time_dif (a1, b1, c1, d1) (a2, b2, c2, d2) = (* time difference between 2 longitude coordinates. format : a° b' c" *)
  let s1 = (a1*240 + b1*4 + c1/15) and s2 = (a2*240 + b2*4 + c2/15) in
  let s1 = if d1 = 'W' then -s1 else s1 and s2 = if d2 = 'W' then -s2 else s2 in
  if s1 > s2 then
    let dif = s1 - s2 in
    let dh, dm, ds = dif / 3600, dif mod 3600 / 60, dif mod 60 in
    "earlier : " ^ string_of_int dh ^ "h " ^ string_of_int dm ^ "m " ^ string_of_int ds ^ "s"
  else 
    let dif = s2 - s1 in
    let dh, dm, ds = dif / 3600, dif mod 3600 / 60, dif mod 60 in
    "later : " ^ string_of_int dh ^ "h " ^ string_of_int dm ^ "m " ^ string_of_int ds ^ "s" ;;
