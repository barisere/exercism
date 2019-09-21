open Base;;

type base = int
;;

(** split_digits generalizes splitting a number into its component digits as
    base conversion. The original digits are maintained if the target base is the same. *)
let rec split_digits ?(base=10) ?(acc=[]) n =
  let (quot, rem) = (n / base, n % base) in
  if quot = 0 then rem::acc else split_digits ~base ~acc:(rem::acc) quot
;;

let to_decimal ~from ~digits = List.fold_left ~f:(fun acc digit -> acc * from + digit) ~init:0 digits
;;

let convert_bases ~from ~digits ~target =
  if List.exists ~f:(fun x -> x < 0 || x >= from) digits then None
  else if from < 2 || target < 2 then None
  else to_decimal ~from ~digits |> split_digits ~base:target |> Option.return
;;

