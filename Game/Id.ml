type t = int

let verbosity = 0

let string_to_id = Hashtbl.create 100
let id_to_string = Hashtbl.create 100
let protected_table: (t, unit) Hashtbl.t = Hashtbl.create 100

let protect (id:t): unit =
    Hashtbl.add protected_table id ()
let protected: t -> bool =
    Hashtbl.mem protected_table

let base = 10
let tags = 5
type tag =
    | Base
    | Applicator
    | Applicand
    | Refinement
    | Constructor
let int_of_tag (tag:tag):int =
    match tag with
    | Base -> 0
    | Applicator -> 1
    | Applicand -> 2
    | Refinement -> 3
    | Constructor -> 4
let tag (tag:tag) (id:t):t =
    id*tags+base+int_of_tag tag
let rec detag (id:t):string list * t =
    if id < base || Hashtbl.mem id_to_string id then [], id else 
    let id = id-base in
    let tag, final_id = detag (id/tags) in
    (match id mod tags with
    | 0 -> "b"
    | 1 -> "f"
    | 2 -> "x"
    | 3 -> "r"
    | 4 -> "c"
    | _ -> "?")::tag, final_id

let unique_counter = ref (0)
let unique (prot:bool) :t = 
    incr unique_counter; 
    let rec f id = function
    | 0 -> id
    | n -> f (tag Base id) (n - 1)
    in
    let out = f (!unique_counter mod base) (!unique_counter / base) in
    (if prot then protect out);
    out


let of_string (str:string):t =
    (* (if String.length str > 1 && String.sub str 0 1 = "_" 
    then failwith ("\""^str^"\" is a reserved identifier because it starts with \"_\".")); *)
    match Hashtbl.find_opt string_to_id str with
    | Some id -> id
    | None -> 
        let id = unique false in
        Hashtbl.add string_to_id str id; 
        Hashtbl.add id_to_string id str; 
        id
    
let output_counter = ref (0)
let to_string (id:t):string = 
    if verbosity = 0 then 
        match Hashtbl.find_opt id_to_string id with
            | Some str -> str
            | None -> incr output_counter; 
                let str = "i" ^ string_of_int !output_counter in
                Hashtbl.add id_to_string id str;
                str
    else
        (let tag, id = detag id in
        let tag = String.concat "" tag in
        match Hashtbl.find_opt id_to_string id with
        | Some str -> if String.length tag = 0 then str else tag^"u_"^str
        | None -> tag^"_"^string_of_int id)
        ^(if verbosity >= 2 then "_["^string_of_int id^"]" else "")