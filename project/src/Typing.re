// File for typechecking.

open Types;

let rec getType = (delta: hole_constraints, gamma: context, e: exp) : type_ =>
    switch(e) {
        | Int(_) => Int_t 
        | Float(_) => Any_t 
        | Bool(_) => Bool_t 
        | Cons(e1, e2) => Cons_t(getType(delta, gamma, e1), getType(delta, gamma, e2))
        | Nil => Any_t 
        | Var(x) => Tools.lookup(x, gamma)
        | Function(id, e') => Function_t(Tools.lookup(id, gamma), getType(delta, gamma, e'))
        | Application(e1, e2) => switch (getType(delta, gamma, e1)) {
            | Function_t(t1, t2) when getType(delta, gamma, e2) == t1 => t2
            | _ => failwith("Application type error")
            }
        | Hole(id) => Tools.lookup(id, delta)
        | Unit => Unit_t 
        | Pair(e1, e2) => Pair_t(getType(delta, gamma, e1), getType(delta, gamma, e2))
        | Fst(e') => switch (e') {
            | Pair(e1, _) => getType(delta, gamma, e1)
            | _ => failwith("Type error, expected pair")
            }
        | Snd(e') => switch(e') {
            | Pair(_, e2) => getType(delta, gamma, e2)
            | _ => failwith("Type error, expected pair")
            }
    };
        
