open Base;;

(** Make a Set containing the characters in s that pass the predicate f *)
let set_of_chars ~f s =
  String.fold ~init:(Set.empty (module Char))
    ~f:(fun acc c -> if f c then Set.add acc c else acc)
    s
;;

(** Concat all characters in set into a string *)
let char_set_to_string set =
  Set.fold ~init:"" ~f:(fun acc e -> acc ^ Char.to_string e) set
;;

(** Extract a string that contains only alphabetic characters *)
let extract_unique_chars s = s |> set_of_chars ~f:Char.is_alpha |> char_set_to_string
;;

(** Compute the sum of the ASCII codes of the characters in a string *)
let sum_chars = String.sum (module Int) ~f:Char.to_int;;

(** Compute the sum of the first n natural numbers *)
let sum_of_first_n n = (n * n + n) / 2;;

(** The sum of the lowercase ASCII character codes *)
let sum_lowercase_alpha =
  sum_of_first_n (Char.to_int 'z') - sum_of_first_n (Char.to_int 'a' - 1);;

let is_pangram s =
  String.lowercase s
  |> extract_unique_chars
  |> sum_chars
  |> (=) sum_lowercase_alpha
;;
