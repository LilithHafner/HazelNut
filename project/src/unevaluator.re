/* Class to house the unevaluator.
    The main function unevaluate
    takes a result and example and
    generates a set of constraints.

    I definitely don't know the best
    way to do this, and it is fully
    incomplete but it doesn't break
    anything so I figured I'd at least
    get it out there if anyone else
    wanted to work on it.

    -Sam
*/

open Draft_of_take_on_Smyth;

type ex =
    | Top 
    | Eunit 
    | Epair(ex, ex)
    | Efunc(value, ex)

and value =
    | Vunit 
    | Vpair(value, value);

let rec valToExp = (v) => {
    switch (v) {
        | Vunit => Unit 
        | Vpair(v1, v2) => Pair(valToExp(v1), valToExp(v2))
        }
};

// This function is totally incorrect
// Needs to only contain top-level some / none, right now even if no corresponding
// value exists it can still return some.
let rec resToVal = (res) => {
    switch (res) {
        | Runit => Some(Vunit)
        | Rpair(r1, r2) => 
            switch ((resToVal(r1), resToVal(r2))) {
                | (Some(x), Some(y)) => Some(Vpair(x, y))
                | _ => None
                }
        | _ => None
        }
};

// Generates constraints from a result and example
let rec unevaluate = (res, ex) => {
    switch ((ex, res)) {
        | (Top, _) => []
        | (Eunit, Runit) => []
        | (Epair(ex1, ex2), Rpair(r1, r2)) => List.concat([unevaluate(r1, ex1), unevaluate(r2, ex2)])
        | (Efunc(v, ex'), Rapp(r1, r2)) => [] 
        | (_, Rhole(env, id)) => [(id, (env, ex))]
        | (_, Rfst(r)) => unevaluate(r, Epair(ex, Top))
        | (_, Rsnd(r)) => unevaluate(r, Epair(Top, ex))
        | (_, Rapp(r1, r2)) => []
        | _ => [] 
    }
};

let rec constrainExp = (exp, exs) => {
    switch (exs) {
        | [] => []
        | [ex, ...xs] => {
            switch (ex) {
                | Efunc(v, ex') => List.concat([unevaluate(Evaluator.eval([], Application(exp, valToExp(v))), ex), constrainExp(exp, xs)])
                | _ => List.concat([unevaluate(Evaluator.eval([], exp), ex), constrainExp(exp, xs)])
                }
        }
    }
};

let rec exToExp = (ex) => {
    switch (ex) {
        | Epair(ex1, ex2) => 
            switch ((exToExp(ex1), exToExp(ex2))) {
                | (Some(x), Some(y)) => Some(Pair(x, y))
                | _ => None
                }
        | Eunit => Some(Unit)
        | _ => None
        }
};

let castable = (res) => 
    switch (resToVal(res)) {
        | Some(_) => true
        | None => false
        }
