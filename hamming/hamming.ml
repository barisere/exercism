type nucleotide = A | C | G | T
;;

let ensure_equal_strands left right =
  match ( List.compare_lengths left right, left, right ) with
  | (0, _, _) -> Ok (left, right)
  | (1, _, []) -> Error "right strand must not be empty"
  | (-1, [], _) -> Error "left strand must not be empty"
  | _ -> Error "left and right strands must be of equal length"
;;

let compute_hamming_distance (left, right): int =
  List.fold_left2 (fun acc n1 n2 -> if n1 = n2 then acc else 1 + acc) 0 left right
;;

let hamming_distance left right =
  Result.map (compute_hamming_distance) (ensure_equal_strands left right)
