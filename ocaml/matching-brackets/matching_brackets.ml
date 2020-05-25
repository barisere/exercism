open Base
;;

let try_pop_char stack c =
  match Stack.pop stack with
  | Some c' when Char.(c' = c) -> Some c
  | Some c' -> Stack.push stack c'; None
  | _ -> None
;;

let are_balanced s =
  let consume_tokens stack s =
    String.fold ~init:(Some "") ~f:(fun acc c ->
        let map_pop_result = Option.map2 ~f:(fun acc' c' ->
            Char.to_string c' ^ acc' ^ Char.to_string c) acc
        in
        match c with
        | '(' | '{' | '[' -> Stack.push stack c; acc
        | ')' -> try_pop_char stack '(' |> map_pop_result
        | '}' -> try_pop_char stack '{' |> map_pop_result
        | ']' -> try_pop_char stack '[' |> map_pop_result
        | _ -> acc
      ) s
  in
  let symbols = Stack.create () in
  let s' = consume_tokens symbols s in
  Stack.is_empty symbols && Option.is_some s'
;;

