let ese w = match w with
  | w when w < 0 -> invalid_arg "weight cant be a negative number"
  | w when w < 500 -> (3.4, 4.6, 9.1)
  | w when w < 1000 -> (4.6, 5.9, 11.)
  | w when w < 2000 -> (5.1, 6.5, 13.5)
  | w when w < 3000 -> (6.9, 7.2, 14.2)
  | _ -> failwith "too heavy" ;;

let rate w t =
  match ese w with
  | (ec, st, ex) -> match t with
    | "eco" -> ec
    | "standard" -> st
    | "express" -> ex
    | _ -> failwith "unknow code" ;;
