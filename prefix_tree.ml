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
    match prefix, s with
    | [], _ -> true
    | _, [] -> false
    | h1 :: t1, h2 :: t2 -> 
      if h1 = h2 then compare t1 t2 
      else false

let rec insert s t = 
  match t with
  | Node (str, tree_list) ->
    insert_helper tree_list s

and insert_helper prefix_list s = 
  match prefix_list with
  | [] -> Node (s, [])
  | Node (h_val, h_lst) :: t ->     
    if compare (str_to_lst h_val []) (str_to_lst s []) (* pre insert scenario , s actually inside prefix *)
    then Node (s, Node (h_val, h_lst) :: t)
    else if compare (str_to_lst s []) (str_to_lst h_val []) (* insert down the line, prefix is in s*)
    then insert s (Node (h_val, h_lst))
    else insert_helper t s (* insert somewhere on t_list, e.g prefix was not a prefix of s *)

let rec compare_lst_dups lst1 lst2 = 
  match lst1, lst2 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | h1 :: t1, h2 :: t2 ->
    if List.mem h1 lst2 && List.mem h2 lst1 then compare_lst_dups t1 t2
    else false

let rec find s t = 
  match t with
  | Node (str, t_list) -> 
    find_helper t_list s 

and find_helper prefix_list s = 
  match prefix_list with
  | [] -> false
  | Node (h_val, h_lst) :: t -> 
    if compare_lst_dups (str_to_lst s []) (str_to_lst h_val []) then 
      true
    else if compare (str_to_lst s []) (str_to_lst h_val []) then 
      find s (Node (h_val, h_lst))
    else 
      find_helper t s

let rec delete s t = 
  failwith "Inimplemented"
