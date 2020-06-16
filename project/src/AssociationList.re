type t('a, 'b) = list(('a, 'b))
let rec lookup(env:t('a, 'b), identifier:'a):'b =
    switch(env) {
    | [] => failwith("Unexpected unbound variable")
    | [(id, value), _] when id == identifier => value
    | [_, ...xs] => lookup(xs, identifier)
    }
