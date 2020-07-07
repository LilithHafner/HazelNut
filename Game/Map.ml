type ('a,'b) t = ('a * 'b) list
let add (map:('a, 'b)t) (key:'a) (value:'b) =
    (key,value)::map
let empty = []
let find = List.assoc
let entries = (fun x -> x)
let mem = List.mem_assoc
