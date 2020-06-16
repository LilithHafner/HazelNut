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

open Types;


// Generates constraints from a result and example
// No constructors in language yet, will probably add later
// But that means there's a bunch less cases
let rec unevaluate = (res, ex) => {
    switch ((ex, res)) {
        | (Top, _) => []
        | (Eunit, Runit) => []
        | (Epair(ex1, ex2), Rpair(r1, r2)) => List.concat([unevaluate(r1, ex1), unevaluate(r2, ex2)])
        | (Efunc(v, ex'), Rapp(r1, r2)) => [] 
        | (_, Rhole(env, id)) => [(id, (env, ex))]
        | (_, Rfst(r)) => unevaluate(r, Epair(ex, Top))
        | (_, Rsnd(r)) => unevaluate(r, Epair(Top, ex))
        | (_, Rapp(r1, r2)) =>
            if (castable(r2)) {
                let Some(v) = resToVal(r2);
                unevaluate(r1, Efunc(v, ex))
            } else {
                [] // fail
            }
        | _ => [] // fail
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


