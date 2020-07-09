open Grammar
open PlayTypes
(* Transition *)
(* Parameters *)
(* Heuristic *)
open Printf

let rec add_antagonist_moves (moves:node list) (assertions:assertion list): node list =
    List.fold_left
        (fun moves assertion -> match Heuristic.heuristic assertion with
            | Some value -> {value; sign=1.; way_down = Leaf (Some assertion); parent=None}::moves
            | None -> (match Transition.apply_inference_rules assertion with
                | Transition.Linked(_, _) -> failwith "Heuristic reported None on a linked choice."
                | Transition.Unlinked(assertionss) -> (List.fold_left 
                    add_antagonist_moves
                    moves
                    assertionss)))
        moves
        assertions
let antagonist_moves = add_antagonist_moves []


let apply_inference_rules (assertion:assertion):(hole * exp list) option * assertion list list = 
    match Transition.apply_inference_rules assertion with
    | Transition.Unlinked(assertionss) -> None, assertionss
    | Transition.Linked(hole, a) -> 
        let choices, assertionss = List.split a in
        Some(hole, choices), assertionss


let down_weight_v sign value = exp(sign*.value/.Parameters.sigma_down)
let up_weight_v sign value = exp(sign*.value/.Parameters.sigma_up)
(* TODO: This is very inneficient. It is a workaround due to 
numerical precision issues. *)
let down_weight sign node = down_weight_v sign node.value
let up_weight sign node = up_weight_v sign node.value
let best_child sign (children:node array):int = let out, _, _ = Array.fold_left
        (fun (bi, bv, i) c -> 
            if c.value *. sign > bv 
            then (i, c.value *. sign, i+1) 
            else (bi, bv, i+1))
    (-1, neg_infinity, 0) children in out
let infer_value sign ({children;link;_}:non_leaf_data) = let out = match link with
    | Linked {selection=Some index;_} -> (* printf "A\n"; *) children.(index).value
    | Linked {selection=None;_} -> (* printf "B\n"; *) -. sign *. Parameters.big_number
    | Unlinked {up_total} -> (* printf "C\n%f\n%f\n" up_total (log up_total);  *)sign *. Parameters.sigma_up *. log
        (max up_total (exp (-.Parameters.big_number/.Parameters.sigma_up)))
    in
    if abs_float (19.4081210557-.out) < 0.001 
    then (printf "\nHere:\n %i\n%f\n%f\n%f\n%f\n%s\n" (Array.length children) 
        (exp (-.Parameters.big_number/.Parameters.sigma_up))
        (sign *. Parameters.sigma_up *. log (max 0. (exp (-.Parameters.big_number/.Parameters.sigma_up))))
        (sign *. Parameters.sigma_up *. log (exp (-.Parameters.big_number/.Parameters.sigma_up)))
        (sign *. -.Parameters.big_number)
        (Up.string_of_list (fun child -> string_of_float child.value) (Array.to_list children))
        ; failwith "here")
    else out
let weighted_choice total weights = 
    let rec f feul index = 
        let feul = feul -. weights.(index) in
        if feul <= 0.
        then index
        else f feul (index+1)
    in f (Random.float total) 0

let sum = Array.fold_left (+.) 0.
let antagonist_dead_end () = {value=Parameters.big_number; sign= -1.; way_down=Leaf None; parent=None}


let links: (Id.t, link_data) Hashtbl.t = Hashtbl.create 100
let counter link = link.counter <- link.counter+1; link.counter

(* Call "add_children node link children" to add children 
to node, give the children parent pointers to node, 
register node as a new member of link (if any), and, 
finally, update the value of node, and if node already 
has a parent, that means also updating ancestor values. *)
let rec add_children (node:node) (link:(hole * exp list) option) (children:node list):unit  =
    let children = Array.of_list children in
    let link = match link with
    | None -> Unlinked {up_total=sum (Array.map (up_weight node.sign) children)}
    | Some(id, choices) -> 
        let table = (Hashtbl.create (List.length choices)) in
        List.iteri (fun i exp -> Hashtbl.add table exp i) choices;
        let link = {id; choices; table; selection = (match Hashtbl.find_opt links id with
            | None -> 
                let selection = best_child node.sign children in
                Hashtbl.add links id {counter=0; choice=List.nth choices selection; members=[]};
                Some selection
            | Some link_data -> Hashtbl.find_opt table link_data.choice)}
        in
        let link_data = Hashtbl.find links id in
        link_data.members <- (node, link)::link_data.members;
        Linked link
    in
    let child_weights = Array.map (down_weight node.sign) children in
    let down_total = sum child_weights in
    let non_leaf_data = {children; down_total; child_weights; link} in
    Array.iteri (fun i child -> child.parent <- Some(node, i, non_leaf_data)) children;
    node.way_down <- Node(non_leaf_data);
    up true node;

(* Call "down node" to think about node *)
and down (node:node):unit =
    if abs_float node.value > Parameters.stop_thinking_threshold
    then (printf "[Play] WARNING: Stopped thinking on the way down due to reaching stop_thinking_threshold, oddly. Perhaps this is on account of following a link. We shouldn't follow forced loss links. We should do a clever little probability that takes the link and discrepancy into account, but we should never end up here. Yet here we are.\n"; up true node)
    else match node.way_down with 
    | Leaf(assertion) -> 
        (* Expand *)
        let link, assertionss = apply_inference_rules (Option.get assertion) in
        add_children node link (List.map (fun a -> match antagonist_moves a with
            | [] -> antagonist_dead_end ()
            | move::[] -> move
            | moves -> let node = antagonist_dead_end () in add_children node None moves; node)
            assertionss)
    | Node({children; down_total; child_weights; link}) -> 
        down children.(match link with 
            | Linked({selection = Some selection}) 
            when Random.float 1. < Parameters.k1 ->
                selection
            | _ -> weighted_choice down_total child_weights)

(* After updating node's children, call "up live node nld" to 
recalculate node's value and propigate those changes to node's parent
(if any) and line of ancetry. 

Precondition: node's way down must be a Node, not a Leaf.*)
and up (live:bool) (node:node):unit =
    let nld = match node.way_down with 
    | Node(nld)->nld 
    | _ -> failwith "[up] precondition violation: node.way_down must be a Node"
    in
    (* Sometimes update a linked node's choice *)
    (match nld.link with
    | Linked link when live && Random.float (float_of_int (counter
            (Hashtbl.find links link.id))) < Parameters.k2 ->
        let selection = best_child node.sign nld.children in
        if Some selection <> link.selection then (
            link.selection <- Some selection;
            let choice = List.nth link.choices selection in
            (Hashtbl.find links link.id).choice <- choice;
            List.iter 
                (fun (n, l) ->
                    let selection =  Hashtbl.find_opt l.table choice in
                    if selection <> l.selection then (
                        l.selection <- selection;
                        up false n))
                (Hashtbl.find links link.id).members)
    | _ -> ());

    (* Recalculate node's value and... *)
    (* let old_value = node.value in *)
    node.value <- infer_value node.sign nld;
    Option.iter (fun (n,i,nld) ->
        (* Update node.parent's non_leaf_data as needed *)
        let sign = n.sign in
        let new_down_weight = down_weight sign node in
        nld.down_total <- nld.down_total 
            -. nld.child_weights.(i) +. new_down_weight;
        nld.child_weights.(i) <- new_down_weight;
        (match nld.link with 
        | Unlinked data -> data.up_total <- (* data.up_total
            -. up_weight_v sign old_value +. up_weight sign node *)
            (* TODO: This is very inneficient. It is a workaround due to 
            numerical precision issues. *)
            sum (Array.map (up_weight sign) nld.children)
        | Linked _ -> ());
        (* Update node.parent's value and ancestry as needed *)
        up live n)
        node.parent;
