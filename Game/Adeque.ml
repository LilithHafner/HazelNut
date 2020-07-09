type 'a t = 'a node Deque.t
and 'a node = 
    | Leaf of 'a 
    | Node of 'a t

let rec pop (t:'a t):'a * 'a t = match Deque.pop_left t with
    | Leaf e, rest -> e, rest
    | Node Deque.Nil, rest -> pop rest
    | Node nn, rest -> 
        let e, nn = Deque.pop_left nn in pop
        (Deque.push_left e (Deque.push_left (Node nn) rest))

let combine (t1:'a t) (t2:'a t):'a t =
    Deque.push_right (Node t2) t1

let singleton (e:'a):'a t =
    Deque.singleton (Leaf e)