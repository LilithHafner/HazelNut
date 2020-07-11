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


let apply_inference_rules (assertion:assertion):(hole * exp list * assertion) option * assertion list list = 
    match Transition.apply_inference_rules assertion with
    | Transition.Unlinked(assertionss) -> None, assertionss
    | Transition.Linked(hole, a) -> 
        let choices, assertionss = List.split a in
        Some(hole, choices, assertion), assertionss


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
let infer_value sign ({children;link;_}:non_leaf_data) = match link with
    | Linked {selection;_} -> children.(selection).value
    | Unlinked {up_total} -> sign *. Parameters.sigma_up *. log
        (max up_total (exp (-.Parameters.big_number/.Parameters.sigma_up)))
let weighted_choice total weights = 
    let rec f feul index = 
        let feul = feul -. weights.(index) in
        if feul <= 0.
        then index
        else f feul (index+1)
    in f (Random.float total) 0

let asum = Array.fold_left (+.) 0.
let lsum = List.fold_left (+.) 0.
let antagonist_dead_end () = {value=Parameters.big_number; sign= -1.; way_down=Leaf None; parent=None}


let links: (Id.t, link_data) Hashtbl.t = Hashtbl.create 100
let counter link = link.counter <- link.counter+1; link.counter


let rec new_child (parent:node) (assertion:assertion) (filling:exp):node =
    let out = antagonist_dead_end () in
    add_children out None 
        (antagonist_moves (Transition.fill_hole_with assertion filling));
    out

and add_child (parent:node) (nld:non_leaf_data) (link:link_link) (filling:exp):unit =
    let child = new_child parent link.assertion filling in
    child.parent <- Some((parent, List.length link.choices, nld));
    Hashtbl.add link.table filling (List.length link.choices);
    link.choices <- link.choices @ [filling];
    nld.children <- Array.init (Array.length nld.children + 1) 
        (fun i -> if i < Array.length nld.children 
            then Array.get nld.children i 
            else child)

(* Call "add_children node link children" to add children 
to node, give the children parent pointers to node, 
register node as a new member of link (if any), and, 
finally, update the value of node, and if node already 
has a parent, that means also updating ancestor values. *)
and add_children (node:node) (link:(hole * exp list * assertion) option) (children:node list):unit  =
    let link_data, children, link = match link with
    | None ->
        None, Array.of_list children, Unlinked {up_total=lsum (List.map (up_weight node.sign) children)}
    | Some(id, choices, assertion) -> 
        let table = (Hashtbl.create (List.length choices)) in
        List.iteri (fun i exp -> Hashtbl.add table exp i) choices;
        let children, choices, selection = match Hashtbl.find_opt links id with
            | None -> 
                let children = Array.of_list children in
                let selection = best_child node.sign children in
                Hashtbl.add links id {counter=0; choice=List.nth choices selection; members=[]};
                children, choices, selection
            | Some {choice;_} -> (match Hashtbl.find_opt table choice with
                | None -> let i = List.length children in
                    Hashtbl.add table choice i;
                    Array.of_list (new_child node assertion choice::children), choices@[choice], i
                | Some selection -> Array.of_list children, choices, selection)
        in
        let link = {id; choices; table; assertion; selection} in
        let link_data = Hashtbl.find links id in
        Some((link_data, link)), children, Linked link
    in
    let child_weights = Array.map (down_weight node.sign) children in
    let down_total = asum child_weights in
    let non_leaf_data = {children; down_total; child_weights; link} in
    Option.iter (fun (link_data, link) -> 
        link_data.members <- (node, non_leaf_data, link)::link_data.members)
        link_data;
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
            | Linked({selection}) 
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
        if selection <> link.selection then (
            link.selection <- selection;
            let choice = List.nth link.choices selection in
            (Hashtbl.find links link.id).choice <- choice;
            List.iter 
                (fun (n, nld, l) ->
                    match Hashtbl.find_opt l.table choice with
                    | None -> 
                        add_child n nld l choice;
                        l.selection <- Array.length nld.children;
                        up false n
                    | Some selection when selection <> l.selection ->
                        l.selection <- selection;
                        up false n
                    | _ -> ())
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
            (* TODO: This is inneficient. It is a workaround due to 
            numerical precision issues. *)
            asum (Array.map (up_weight sign) nld.children)
        | Linked _ -> ());
        (* Update node.parent's value and ancestry as needed *)
        up live n)
        node.parent;
