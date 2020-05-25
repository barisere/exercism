let transform letters_per_score =
  List.fold_left (fun acc (score, letters) ->
      List.fold_left (fun acc' l -> (Char.lowercase_ascii l, score)::acc')
        acc
        letters)
    []
    letters_per_score
|> List.sort (fun (n, _) (n', _) -> Char.compare n n')

