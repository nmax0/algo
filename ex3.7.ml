let rate w t =match t with
  | "eco" -> (match w with
      | w when w < 500 -> 3.4
      | w when w < 1000 -> 4.6
      | w when w < 2000 -> 5.1
      | w when w < 3000 -> 6.9
      | _ -> failwith "too heavy")
  | "standard" -> (match w with
      | w when w < 500 -> 4.6
      | w when w < 1000 -> 5.9
      | w when w < 2000 -> 6.5
      | w when w < 3000 -> 7.2
      | _ -> failwith "too heavy")
  | "express" -> (match w with
      | w when w < 500 -> 9.1
      | w when w < 1000 -> 11.
      | w when w < 2000 -> 13.5
      | w when w < 3000 -> 14.2
      | _ -> failwith "too heavy")
  | _ -> failwith "unknow code" ;;
