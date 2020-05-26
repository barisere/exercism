open Base
open Int64

let base_numbers =
  Hashtbl.of_alist_exn
    (module Int64)
    [ 0L, "zero"
    ; 1L, "one"
    ; 2L, "two"
    ; 3L, "three"
    ; 4L, "four"
    ; 5L, "five"
    ; 6L, "six"
    ; 7L, "seven"
    ; 8L, "eight"
    ; 9L, "nine"
    ; 10L, "ten"
    ; 11L, "eleven"
    ; 12L, "twelve"
    ; 13L, "thirteen"
    ; 14L, "fourteen"
    ; 15L, "fifteen"
    ; 16L, "sixteen"
    ; 17L, "seventeen"
    ; 18L, "eighteen"
    ; 19L, "nineteen"
    ; 20L, "twenty"
    ; 30L, "thirty"
    ; 40L, "forty"
    ; 50L, "fifty"
    ; 60L, "sixty"
    ; 70L, "seventy"
    ; 80L, "eighty"
    ; 90L, "ninety"
    ]
;;

let n_in_words n =
  let units_and_tens = function
    | 0L -> ""
    | n when n < 20L -> Hashtbl.find_exn base_numbers n
    | n ->
      let quot, remainder = n / 10L * 10L, n % 10L in
      let suffix =
        if remainder = 0L then "" else "-" ^ Hashtbl.find_exn base_numbers remainder
      in
      Hashtbl.find_exn base_numbers quot ^ suffix
  in
  let hundreds = function
    | 0L -> ""
    | n ->
      let quot, remainder = n / 100L, n % 100L in
      let prefix =
        if quot = 0L then "" else Hashtbl.find_exn base_numbers quot ^ " hundred"
      in
      let suffix = if remainder = 0L then "" else " " ^ units_and_tens remainder in
      prefix ^ suffix
  in
  let div_hundreds_with_remainder num div =
    let quot, remainder = num / div, num % div in
    if quot = 0L then "", remainder else hundreds quot, remainder
  in
  let prefix_place_value amount order =
    if String.(amount = "") then "" else amount ^ " " ^ order
  in
  let billion, million_rem = div_hundreds_with_remainder n 1_000_000_000L in
  let million, thousand_rem = div_hundreds_with_remainder million_rem 1_000_000L in
  let thousand, hundred_rem = div_hundreds_with_remainder thousand_rem 1_000L in
  let hundred = hundreds hundred_rem in
  List.fold
    ~init:""
    ~f:(fun acc v -> acc ^ " " ^ String.strip v)
    [ prefix_place_value billion "billion"
    ; prefix_place_value million "million"
    ; prefix_place_value thousand "thousand"
    ; hundred
    ]
  |> String.strip
;;

let in_english number =
  if number < 0L || number > 999_999_999_999L
  then Error "input out of range"
  else if number < 20L
  then Ok Hashtbl.(find_exn base_numbers number)
  else Ok (n_in_words number)
;;
