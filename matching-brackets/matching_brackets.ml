open Base
;;

let try_pop_char stack c =
  match Stack.pop stack with
  | Some c' when Char.(c' = c) -> Some c
  | Some c' -> Stack.push stack c'; None
  | _ -> None
;;

let lex_brackets s =
  let consume_tokens stack s =
    String.foldi ~init:(Ok s) ~f:(fun i acc c ->
        let map_pop_error = function
          | Some _ -> acc
          | None -> Error(i, c)
        in
        if Result.is_error acc then acc else
          match c with
          | '(' | '{' | '[' -> Stack.push stack c; acc
          | ')' -> try_pop_char stack '(' |> map_pop_error
          | '}' -> try_pop_char stack '{' |> map_pop_error
          | ']' -> try_pop_char stack '[' |> map_pop_error
          | _ -> acc
      ) s
  in
  let symbols = Stack.create () in
  Result.(consume_tokens symbols s >>= (fun s' -> match Stack.top symbols with
      | Some s -> Error(0, s)
      | None -> Ok s'))
;;

let are_balanced s = Result.is_ok (lex_brackets s)
;;

