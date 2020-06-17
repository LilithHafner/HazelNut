/* Class to house the unevaluator.
    The main function unevaluate
    takes a result and example and
    generates a set of constraints.
*/

open Types;


// Generates constraints from a result and example
// No constructors in language yet, will probably add later
// But that means there's a bunch less cases
// I believe this is all cases without adding in constructors
let rec unevaluate = (res:res, ex:example) => {
    switch ((ex, res)) {
        // Top adds no constraints
        | (Top, _) => []
        // Matching constants adds no constraints
        | (Eunit, Runit) => []
        // A pair of examples adds the constraints of both examples
        // on their respective results
        | (Epair(ex1, ex2), Rpair(r1, r2)) => List.concat([unevaluate(r1, ex1), unevaluate(r2, ex2)])
        // A hole adds its environment and example to the list of 
        // unfilled holes.
        | (_, Rhole(env, id)) => [(id, (env, ex))]
        | (_, Rfst(r)) => unevaluate(r, Epair(ex, Top))
        | (_, Rsnd(r)) => unevaluate(r, Epair(Top, ex))
        // Attempts to cast r2 to a value, and then continues
        // unevaluation on r1 with a new example.
        | (_, Rapp(r1, r2)) =>
            if (castable(r2)) {
                let Some(v) = resToVal(r2);
                unevaluate(r1, Efunc(v, ex))
            } else {
                [] // fail
            }
        // Takes and input-output example and a lambda result,
        // and binds the variable of the lambda to the value,
        // and then performs bidirectional evaluation.
        | (Efunc(v, ex'), Rfunc(id, exp, env)) => {
            let env' = [(id, valToRes(v)), ...env];
            let exs = [(env', ex')];
            constrainExp(exp, exs)
        }
        // When none of the inference rules apply
        | _ => failwith("Unevaluation failed") // fail
    }
}

and constrainExp = (exp, exs) => {
    switch (exs) {
        | [] => []
        | [(env, ex), ...xs] => List.concat([unevaluate(Evaluator.eval(env, exp), ex), constrainExp(exp, xs)])
    }
};


