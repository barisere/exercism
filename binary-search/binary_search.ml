open Result

let find elements e =
  let rec search = function
    | [| |] -> error "value not in array"
    | [| x |] -> if x = e then ok 0 else error "value not in array"
    | arr ->
      let n = Array.length arr in
      let mid_value = arr.(n / 2) in
      if e < mid_value then
        Array.sub arr 0 (n / 2) |> search
      else
        Array.sub arr (n / 2) (n - n / 2) |> search |> Result.map (Int.add (n / 2))
  in
  search elements
