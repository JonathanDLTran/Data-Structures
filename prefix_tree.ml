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
  match prefix, s with
  | [], _ -> true
  | _, [] -> false
  | h1 :: t1, h2 :: t2 -> 
    if h1 = h2 then compare t1 t2 
    else false

let rec collect_nodes s prefix_list acc1 acc2 = 
  match prefix_list with 
  | [] -> (acc1, acc2)
  | Node (h_val, h_lst) :: t -> 
    if compare (str_to_lst h_val []) (str_to_lst s []) 
    then collect_nodes s t (Node (h_val, h_lst) :: acc1) acc2
    else collect_nodes s t acc1 (Node (h_val, h_lst) :: acc2)

let rec insert s t = 
  match t with
  | Node (str, tree_list) ->
    Node (str, insert_helper tree_list s)

and insert_helper prefix_list s = 
  match prefix_list with
  | [] -> [ Node(s, []) ] 
  | Node (h_val, h_lst) :: t ->     
    if compare (str_to_lst h_val []) (str_to_lst s []) (* pre insert scenario , s actually prefix of the prefix *)
    then  
      let (success, failure) = collect_nodes s prefix_list [] [] in (* collect all possible prefixes *)
      Node (s, success ) :: failure  
    else if compare (str_to_lst s []) (str_to_lst h_val []) (* insert down the line, prefix is in s*)
    then (insert s (Node (h_val, h_lst)) ) :: t
    else Node (h_val, h_lst) :: insert_helper t s (* insert somewhere on t_list, e.g prefix was not a prefix of s *)

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
    if s = str then true else
      find_helper t_list s 

and find_helper prefix_list s = 
  match prefix_list with
  | [] -> false
  | Node (h_val, h_lst) :: t -> 
    (* if compare_lst_dups (str_to_lst s []) (str_to_lst h_val []) then 
       true
       else *)
    if compare (str_to_lst s []) (str_to_lst h_val []) then 
      find s (Node (h_val, h_lst))
    else 
      find_helper t s

let rethread_tree t_list = 
  match t_list with
  | [] -> []
  | Node (h_val, h_list) :: t_list ->
    Node 

let delete s t = 
  if s = "" then t else
    match t with
    | Node (str, t_list) -> 
      let rec delete_lower s t = 


      and delete_helper prefix_list s = 






