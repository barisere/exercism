open Base;;

type base = int
;;

let rec split_digits ?(base=10) ?(acc=[]) n =
  let (quot, rem) = (n / base, n % base) in
  if quot = 0 then rem::acc else split_digits ~base ~acc:(rem::acc) quot
;;

let to_base_10 ~from ~digits =
  if List.exists ~f:(fun x -> x >= from || x < 0) digits then None
  else let length = List.length digits in
    let radix_to_pow pos n = n * from ** (length - pos - 1) in
    List.foldi ~init:0 ~f:(fun pos acc x -> acc + radix_to_pow pos x) digits
    |> split_digits
    |> Option.return
;;

let from_base_10 ~target ~digits =
  if List.exists ~f:(fun x -> x < 0) digits then None
  else List.fold_left ~f:(fun acc x -> acc ^ Int.to_string x) ~init:"" digits
       |> Int.of_string
       |> split_digits ~base:target
       |> Option.return
;;


let convert_bases ~from ~digits ~target =
  match (from, target) with
  | (f, t) when f < 2 || t < 2 -> None
  | (_, 10) -> to_base_10 ~from ~digits
  | (10, _) -> from_base_10 ~target ~digits
  | _ -> Option.(to_base_10 ~from ~digits >>= (fun x -> from_base_10 ~target ~digits:x))
;;
