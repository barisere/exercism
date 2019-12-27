exception Underflow

let recite from until =
  let rec do_count_up f acc nth = if nth > until then acc else do_count_up f (f (from - until + nth)::acc) (nth + 1) in
  let beer_verse_template = Printf.sprintf "%s of beer on the wall, %s of beer.\nTake %s down and pass it around, %s of beer on the wall.\n"
  in
  let beer_verse = function
    | 0 -> "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall."
    | 1 -> beer_verse_template "1 bottle" "1 bottle" "it" "no more bottles"
    | 2 -> beer_verse_template "2 bottles" "2 bottles" "one" "1 bottle"
    | n when n > 2 ->
      let s1, s2 = Printf.(sprintf "%d bottles" n, sprintf "%d bottles" (n - 1)) in beer_verse_template s1 s1 "one" s2
    | _ -> raise Underflow
  in
  do_count_up beer_verse [] 1 |> String.concat "\n" |> String.trim
