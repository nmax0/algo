(* 5. Lists & tuples *)

(* 5.1 : simulate dictionary *)
let rec assoc k l =
  match l with
  | (key, v)::tl->
      if key = k then v
      else if key > k then failwith "not found"
      else assoc k tl
  | [] -> failwith "not found" ;;

(* 5.2 : flat a linked list into a single list *)
(* with '@' *)
let rec flatten l =
  match l with
  | e::l -> e@flatten l
  | [] -> [] ;;
(* without '@'.. it just doesnt work except if every list contains a single element *)
let rec flatten l =
  match l with
  | e::l -> begin match e with
      | e::l' -> e::flatten l
      | _ -> [] end
  | _ -> [] ;;
