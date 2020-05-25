open Base

let lookup_table =
  [ 1000, "M"
  ; 900, "CM"
  ; 500, "D"
  ; 400, "CD"
  ; 100, "C"
  ; 90, "XC"
  ; 50, "L"
  ; 40, "XL"
  ; 10, "X"
  ; 9, "IX"
  ; 5, "V"
  ; 4, "IV"
  ; 1, "I"
  ]
;;

let to_roman n =
  let get_roman decimal_n (roman_place_value, string_value) =
    let quot, rem = decimal_n / roman_place_value, decimal_n % roman_place_value in
    if quot = 0
    then decimal_n, ""
    else rem, List.init quot ~f:(Fn.const string_value) |> String.concat
  in
  let _, roman_string =
    List.fold
      ~init:(n, "")
      ~f:(fun (n, roman_s) v ->
        let next_n, n_in_roman = get_roman n v in
        next_n, roman_s ^ n_in_roman)
      lookup_table
  in
  roman_string
;;
