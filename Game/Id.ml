type t = int

let base = 10
let tags = 4
type tag =
    | Base
    | Applicator
    | Applicand
    | Refinement
let int_of_tag (tag:tag):int =
    match tag with
    | Base -> 0
    | Applicator -> 1
    | Applicand -> 2
    | Refinement -> 3
let tag (tag:tag) (id:t):t =
    id*tags+base+int_of_tag tag


let unique_counter = ref (0)
let unique ():t = 
    incr unique_counter; 
    let rec f id = function
    | 0 -> id
    | n -> f (tag Base id) (n - 1)
    in
    f (!unique_counter mod base) (!unique_counter / base)


let string_to_id = Hashtbl.create 100
let id_to_string = Hashtbl.create 100
let of_string (str:string):t =
    (if String.length str > 1 && String.sub str 0 1 = "_" 
    then failwith ("\""^str^"\" is a reserved identifier because it starts with \"_\"."));
    match Hashtbl.find_opt string_to_id str with
    | Some id -> id
    | None -> 
        let id = unique () in
        Hashtbl.add string_to_id str id; 
        Hashtbl.add id_to_string id str; 
        id
let output_counter = ref (0)
let to_string (id:t):string = 
    match Hashtbl.find_opt id_to_string id with
    | Some str -> str
    | None -> incr output_counter; 
        let str = "_" ^ string_of_int !output_counter in
        Hashtbl.add id_to_string id str;
        str^"_["^string_of_int id^"]"
