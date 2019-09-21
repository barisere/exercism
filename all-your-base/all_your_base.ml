open Base;;

type base = int
;;

let rec split_digits ?(base=10) ?(acc=[]) n =
  let (quot, rem) = (n / base, n % base) in
  if quot = 0 then rem::acc else split_digits ~base ~acc:(rem::acc) quot
;;

let to_base_10 ~from ~digits =
  let length = List.length digits in
  let radix_to_pow pos n = n * from ** (length - pos - 1) in
  List.foldi ~init:0 ~f:(fun pos acc x -> acc + radix_to_pow pos x) digits
  |> split_digits
;;

let from_base_10 ~target ~digits =
  List.fold_left ~f:(fun acc x -> acc ^ Int.to_string x) ~init:"" digits
  |> Int.of_string
  |> split_digits ~base:target
;;


let convert_bases ~from ~digits ~target =
  if List.exists ~f:(fun x -> x < 0 || x >= from) digits then None
  else if from < 2 || target < 2 then None
  else let conv =
         match (from, target) with
         | (_, 10) -> to_base_10 ~from ~digits
         | (10, _) -> from_base_10 ~target ~digits
         | _ -> to_base_10 ~from ~digits |> (fun x -> from_base_10 ~target ~digits:x)
    in
    Some conv
;;

