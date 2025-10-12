let count_pos lst k = (* 3.6.1 *)
  if k > 0 then
    let rec aux lst acc =
      match lst with
      | e::l when e > 0 -> aux l (acc+1)
      | e::l -> aux l acc
      | [] -> acc < k in
    aux lst 0
  else invalid_arg "k <= 0 or empty list" ;;
  
