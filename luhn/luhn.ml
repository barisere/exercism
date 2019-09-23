open Base
;;

let double_second idx v =
  if idx % 2 = 0 then
    let d = v + v in
    if d > 9 then d - 9 else d
  else v

let valid id_number =
  if String.exists
      ~f:(fun c -> not(Char.is_digit c || Char.is_whitespace c)) id_number
  then false
  else let id_number = String.filter ~f:Char.is_digit id_number in
    if String.length id_number < 2 then false
    else id_number
         |> String.to_list_rev
         |> List.mapi ~f:(fun idx v -> Char.get_digit_exn v |> double_second (idx + 1))
         |> List.fold ~init:0 ~f:(+)
         |> fun v -> v % 10 = 0
;;

