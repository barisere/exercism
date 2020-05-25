open Base;;

let increment_key map s =
  let count =
    match Map.find map s with
    | None   -> 0
    | Some x -> x
  in
  Map.set map ~key:s ~data:(count + 1)
;;

let rec anagrams word = function
  | []                                                       -> []
  | word'::tl when String.(lowercase word = lowercase word') -> anagrams word tl
  | word'::tl when String.length word = String.length word'  ->
    let char_counts = String.fold ~init:(Map.empty (module Char)) ~f:increment_key (String.lowercase word) in
    let char_counts' = String.fold ~init:(Map.empty (module Char)) ~f:increment_key (String.lowercase word') in
    if Map.compare_direct Int.compare char_counts' char_counts = 0
    then word'::(anagrams word tl)
    else anagrams word tl
  | _::tl                                                    -> anagrams word tl
;;
