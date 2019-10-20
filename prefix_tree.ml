type trie = 
  | Node of string * (trie list)

let empty = Node ("", [])

let rec str_to_lst s acc = 
  match String.get s 0 with
  | exception (Invalid_argument _ )-> List.rev (List.map Char.escaped acc)
  | c -> str_to_lst (String.sub s 1 ((String.length s) - 1) ) (c :: acc)

(** [compare s prefix] is [true] if [s] contains [prefix] as a prefix,
    [false] otherwise. *)
let rec compare s prefix = 
  if prefix = [""] then true else
    match prefix with
    | [] -> true
    | h :: t -> 
      if List.mem h s then compare t prefix 
      else false

let rec insert s t = 
  match t with
  | Node (str, tree_list) ->
    match_prefix_w_string tree_list s

and match_prefix_w_string prefix_list s = 
  match prefix_list with
  | [] -> Node (s, [])
  | Node (h_val, h_lst) :: t -> 
    if compare (str_to_lst s []) (str_to_lst h_val []) 
    then insert s (Node (h_val, h_lst))
    else match_prefix_w_string t s

