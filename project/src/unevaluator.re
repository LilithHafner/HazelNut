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
// I believe this is all cases without adding in constructors
let rec unevaluate = (res:res, ex:example) => {
    switch ((ex, res)) {
        | (Top, _) => []
        | (Eunit, Runit) => []
        | (Epair(ex1, ex2), Rpair(r1, r2)) => List.concat([unevaluate(r1, ex1), unevaluate(r2, ex2)])
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
        | (Efunc(v, ex'), Rfunc(id, exp, env)) => {
            let env' = [(id, valToRes(v)), ...env];
            let exs = [(env', ex')];
            constrainExp(exp, exs)
        }
        | _ => [] // fail
    }
}

and constrainExp = (exp, exs) => {
    switch (exs) {
        | [] => []
        | [(env, ex), ...xs] => List.concat([unevaluate(Evaluator.eval(env, exp), ex), constrainExp(exp, xs)])
    }
};


