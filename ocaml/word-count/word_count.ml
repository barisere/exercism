open Base

let unquote s = String.(
    let q = String.of_char '\'' in
    if is_prefix s ~prefix:q && is_suffix s ~suffix:q
    then drop_prefix s 1 |> Fn.flip drop_suffix 1
    else s)

let word_count line =
  let words = String.split_on_chars ~on:[' ';'\n';',';'.';'!';'^';'@';'$';'%';'&';':'] line
              |> List.filter_map ~f:(fun s ->
                  String.(if exists ~f:Char.is_alphanum s then Some (unquote s |> lowercase) else None))
  in
  let word_counts = Map.empty (module String) in
  let incr_opt = function
    | Some i -> Some (i + 1)
    | None -> Some 1
  in
  List.fold ~init: word_counts ~f:(fun m word -> Map.change m word ~f:incr_opt) words
