type 'a t = 
    | Nil
    | Nonempty of 'a list

let push_left (element:'a) (deque:'a t):'a t = failwith "undefined"
let pop_left (deque:'a t):'a * 'a t = failwith "undefined"
let push_right (element:'a) (deque:'a t):'a t = failwith "undefined"
let pop_right (deque:'a t):'a * 'a t = failwith "undefined"
let singleton (element:'a):'a t = failwith "undefined"