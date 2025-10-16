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
(* without '@' *)
let rec flatten ll =
  match ll with
  | (e::l)::ll -> e::flatten(l::ll)
  | e::ll -> flatten ll
  | [] -> [] ;;
(* with '@' *)
let rec flatten l =
  match l with
  | e::l -> e@flatten l
  | [] -> [] ;;
