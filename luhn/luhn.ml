open Base
;;

let double_second idx v =
  if idx % 2 = 0 then
    let d = v + v in
    if d > 9 then d - 9 else d
  else v

let valid id_number =
  if String.exists id_number ~f:(fun c ->
      not(Char.is_digit c || Char.is_whitespace c))
  then false
  else
    let id_number = String.filter ~f:Char.is_digit id_number in
    let len = String.length id_number in
    if len < 2 then false
    else id_number
         |> String.foldi ~init:0 ~f:(fun idx acc v ->
             Char.get_digit_exn v
             |> double_second (len - idx)
             |> (+) acc)
         |> fun v -> v % 10 = 0
;;

