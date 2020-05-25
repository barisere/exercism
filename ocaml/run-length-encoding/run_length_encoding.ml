let encode char_str =
  if String.length char_str <= 1 then char_str else
    let last_char, str, repeats = ref char_str.[0], ref "", ref 1 in
    let write_encoding () =
      let str' = String.make 1 !last_char in
      str := if !repeats = 1 then !str ^ str' else !str ^ string_of_int !repeats ^ str'
    in
    begin
      for idx = 1 to String.length char_str - 1 do
        let c = char_str.[idx] in
        if c = !last_char then incr repeats
        else
          (
            write_encoding ();
            last_char := c;
            repeats := 1
          )
      done;
      write_encoding ();
    end;
    !str

let decode char_str =
  let digit, str = ref "", ref "" in
  let get_digit () = if !digit = "" then 1 else int_of_string !digit in
  begin
    for idx = 0 to String.length char_str - 1 do
      let c = char_str.[idx] in
      if c < '0' || c > '9' then
        (
          str := !str ^ String.make (get_digit ()) c;
          digit := ""
        )
      else digit := !digit ^ String.make 1 c
    done
  end;
  !str
