let rec lookup(env, identifier) =
    switch(env) {
    | [] => failwith("Unbound variable")
    | [(id, value), _] when id == identifier => value
    | [_, ...xs] => lookup(xs, identifier)
    }
