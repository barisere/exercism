open Base;;

type base = int
;;

let to_base_10 ~from ~digits =
  match from with
  | n when n > 1 ->
    if List.exists ~f:(fun x -> x >= from || x < 0) digits
    then None
    else let length = List.length digits in
      let radix_to_pow pos i = i * n ** (length - pos - 1) in
      Some (List.foldi ~init:0 ~f:(fun pos acc x -> (radix_to_pow pos x)+acc) digits |> Int.to_string |> String.to_list |> List.map ~f:Char.get_digit_exn)
  | _ -> None
;;

let from_base_10 ~target ~digits =
  match target with
  | n when n > 1 ->
    if List.exists ~f:(fun x -> x < 0) digits
    then None
    else let number = List.fold_left ~f:(fun acc x -> acc ^ Int.to_string x) ~init:"" digits |> Int.of_string in
    let rec divide acc i =
      let (quot, rem) = (i / n, i % n) in
      if quot = 0 then rem::acc else divide (rem::acc)quot
    in
    Some (divide [] number)
  | _ -> None
;;


let convert_bases ~from ~digits ~target =
  match (from, target) with
  | (_, 10) -> to_base_10 ~from ~digits
  | (10, _) -> from_base_10 ~target ~digits
  | _ -> let new_digits = to_base_10 ~from ~digits in
    match new_digits with
      Some x -> from_base_10 ~target ~digits:(x)
    | None -> None
;;
