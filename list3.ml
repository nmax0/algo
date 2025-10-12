(* 3. Lists & orders *)

(* 3.4 : remove l dupes *)
let del_dupes l =
  let rec mem x l =
    match l with
    | e::l when x = e -> true
    | e::l -> mem x l
    | _ -> false in
  let rec aux l acc =
    match l with
    | e::l when mem e acc -> aux l acc
    | e::l -> e::aux l (e::acc)
    | _ -> [] in
  aux l [] ;;

(* 3.5.1 : list reverse with @ *)
let rec reverse l =
  match l with
  | e::l -> (reverse l)@[e]
  | [] -> [] ;;

(* 3.5.2 : list reverse without @ *)
let reverse l =
  let rec aux l acc =
    match l with
    | e::l -> aux l (e::acc)
    | [] -> acc in
  aux l [] ;;

(* 3.6.1 : less positive number in l than k *)
let count_pos l k =
  if k > 0 then
    let rec aux l acc =
      match lst with
      | e::l when e > 0 -> aux l (acc+1)
      | e::l -> aux l acc
      | [] -> acc < k in
    aux l 0
  else invalid_arg "k <= 0" ;;
  
