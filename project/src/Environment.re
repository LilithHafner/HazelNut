let rec lookup(env, identifier) =
    switch(env) {
    | [] => failwith("Unexpected unbound variable")
    | [(id, value), _] when id == identifier => value
    | [_, ...xs] => lookup(xs, identifier)
    }
