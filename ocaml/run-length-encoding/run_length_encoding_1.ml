let rec count_chars ?(acc = []) s =
  let len = String.length s in
  if len = 0 then acc else
    let sub = String.sub s 1 (len - 1) in
    match s.[0], acc with
    | c, (n, c')::acc ->
      let acc = if Char.equal c c' then (n + 1, c)::acc else (1, c)::(n, c')::acc in
      count_chars ~acc sub
    | c, [] -> count_chars ~acc:[(1, c)] sub

let combine_chars cs = List.fold_right (fun (n, c) s ->
    if n = 1 then s ^ String.make 1 c else
      s ^ string_of_int n ^ String.make 1 c
  ) cs ""

let encode char_str =
  count_chars char_str |> combine_chars

let decode char_str =
  let digit = ref "" in
  let str = ref "" in
  let get_digit () = if !digit = "" then 1 else int_of_string !digit in
  begin
    for idx = 0 to String.length char_str - 1 do
      let c = char_str.[idx] in
      if c < '0' || c > '9'
      then
        (
          str := !str ^ String.make (get_digit ()) c;
          digit := ""
        )
      else
        digit := !digit ^ String.make 1 c
    done
  end;
  !str
