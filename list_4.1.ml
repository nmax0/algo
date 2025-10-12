(* ex 4.1 : equality between 2 lists *)
let rec equality l1 l2 =
  match l1, l2 with
  | e1::l1, e2::l2 when e1 = e2 -> equal l1 l2
  | [], [] -> true
  | _ -> false ;;

(* 4.2 : dupes between 2 lists *)
let rec dupes l1 l2 =
  match l1, l2 with
  | e1::l1, e2::l2 when e1 = e2 -> e1::dupes l1 l2
  | e1::l1, e2::l2 when e1 > e2 -> dupes (e1::l1) l2
  | e1::l1, l2 -> dupes l1 l2
  | _ -> [] ;;

(* 4.3 : is l1 a sublist of l2 *)
let sublist l1 l2 =
  let sl1 = l1 in (* initial l1 *)
  let rec aux l1 l2 =
    match l1, l2 with
    | e1::tl1, e2::tl2 when e1 = e2 -> aux tl1 tl2
    | e1::tl1, e2::tl2 -> (
        match l1 with
        | l1 when l1 = sl1 -> aux l1 tl2
        | _ -> aux sl1 l2 )
    | [], _ -> 1 + aux sl1 l2
    | _, [] -> 0 in
  if l1 = [] then invalid_arg "empty list" else aux l1 l2 ;; 
