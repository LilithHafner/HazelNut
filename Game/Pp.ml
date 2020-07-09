open Grammar
open Up
open PlayTypes

let string_of_exp = Ppp.string_of_exp
let string_of_annotation = Ppp.string_of_annotation
let string_of_env = Ppp.string_of_env
let string_of_qexp = Ppp.string_of_qexp
let string_of_assertion = Ppp.string_of_assertion


let string_of_fillings x = 
    List.sort compare x |>
    string_of_fillings |>
    Str.global_replace (Str.regexp "    (\\([^,]+\\), \\([^;]+\\));?\n") "  \\1 -> \\2\n"
    


let string_of_tree (label_and_children_of_node:'a -> string list * 'a list) (root:'a):string =
    let rec r newline node = 
        let label, children = label_and_children_of_node node in
        newline ^ (String.concat newline label) ^ 
        (String.concat "" (match List.rev children with 
            | [] -> []
            | last::front -> List.rev_append
                (List.map 
                    (r (newline ^ " |")) 
                    (List.rev front))
                ((r (newline ^ "  ") last)::[])))
    in r "\n" root

(* let string_of_node = string_of_tree (fun node ->
    let children, children_terminal = match node.way_down with
    | Leaf(assertion) -> [], String.split_on_char '\n' (string_of_option string_of_assertion assertion)
    | Node(non_leaf_data) -> Array.to_list non_leaf_data.children, []
    in
    ((string_of_float node.value)::children_terminal),
    children
) *)

let string_of_sign sign = if sign = 1. then "Protagonist" else if sign = -1. then "Antagonist" else "???"

type tagged_node = string list * node
let p_string_of_node node = string_of_tree (fun (tag, node) ->
    let label = string_of_float node.value ^ " " ^ string_of_sign node.sign in
    let label, children = (match node.way_down with
    | Leaf(assertion) -> (match assertion with 
        | None -> [label^" dead end."]
        | Some assertion -> (label^" assertion to explore:")::
            (List.map 
                (fun s -> "  "^s) 
                (String.split_on_char '\n' (string_of_assertion assertion)))), []
    | Node(non_leaf_data) -> 
        let desc, children = match Array.to_list non_leaf_data.children with
        | [] -> " dead end", []
        | c -> " to move", c
        in
        let link, choices = match non_leaf_data.link with
        | Unlinked(_) -> "", List.map (fun _ -> []) children
        | Linked({id;choices;_}) -> let id = string_of_id id in
            (" (id = "^id^")"), (List.map (fun exp -> [id^" -> "^string_of_exp exp]) choices)
        in
        [label^desc^link], List.combine choices children)
    in
    tag@label, children)
    ([], node)
