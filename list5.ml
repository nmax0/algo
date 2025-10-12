(* 5. Lists & tuples *)

(* 5.1 : simulate dictionary *)
let rec assoc k l =
  match l with
  | (key, v)::tl->
      if key = k then v
      else if key > k then failwith "not found"
      else assoc k tl
  | [] -> failwith "not found" ;;
