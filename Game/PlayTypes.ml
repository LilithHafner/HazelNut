open Grammar

type node = {
    mutable value: float; (* When you update this, also update the parent's total trackers. *)
    sign: float;
    mutable way_down: way_down;
    parent: parent;
}
and way_down = 
    | Leaf of assertion option
    | Node of non_leaf_data
and parent = (node * int * non_leaf_data) option
and non_leaf_data = {
    children: node array;
    mutable down_total: float;
    child_weights: float array;
    link: link;
}
and link = 
    | Unlinked of up_total
    | Linked of link_link
and up_total = {mutable up_total:float}
and link_link = {
    id: Id.t;
    choices: exp list;
    table: (exp, int) Hashtbl.t;
    mutable selection: int option;
}
type link_data = {
    mutable counter:int;
    mutable choice:exp;
    mutable members:(node * link_link) list;
}