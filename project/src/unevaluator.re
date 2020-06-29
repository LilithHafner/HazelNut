/* Class to house the unevaluator.
    The main function unevaluate
    takes a result and example and
    generates a set of constraints.
*/

open Types;

// constraints = (U, F)
// U = list((id, (env, ex)))
// result r, example ex => (U, F)

// call uneval on different results and examples.
// r = ([E] ??1, ()), ex = () => ([(1, (E, ()))], -) 

// Generates constraints from a result and example
// No constructors in language yet, will probably add later
// But that means there's a bunch less cases
// I believe this is all cases without adding in constructors
let rec unevaluate = (res:res, ex:example) : option(unevalcons) => {
    switch ((ex, res)) {
        // Top adds no constraints
        | (Top, _) => Some(([], []))
        // Matching constants adds no constraints
        | (Eunit, Runit) => Some(([], []))
        | (Eint(x), Rint(y)) when x == y => Some(([], []))
        | (Ebool(x), Rbool(y)) when x == y => Some(([], []))
        // A pair of examples adds the constraints of both examples
        // on their respective results
        | (Epair(ex1, ex2), Rpair(r1, r2)) => {
            switch (unevaluate(r1, ex1), unevaluate(r2, ex2)) {
                | (Some((k1, _)), Some((k2, _))) => Some((List.concat([k1, k2]), []))
                | _ => None
                }
        }
        // A hole adds its environment and example to the list of 
        // unfilled holes.
        | (_, Rhole(id, env)) => Some(([(id, [(env, ex)])], []))
        | (_, Rfst(r)) => unevaluate(r, Epair(ex, Top))
        | (_, Rsnd(r)) => unevaluate(r, Epair(Top, ex))
        // Attempts to cast r2 to a value, and then continues
        // unevaluation on r1 with a new example.
        | (_, Rapp(r1, r2)) =>
            if (castable(r2)) {
                let Some(v) = resToVal(r2);
                unevaluate(r1, Efunc(v, ex))
            } else {
                None // fail
            }
        // Takes and input-output example and a lambda result,
        // and binds the variable of the lambda to the value,
        // and then performs bidirectional evaluation.
        | (Efunc(v, ex'), Rfunc(id, _, exp, env)) => {
            let env' = [(id, valToRes(v)), ...env];
            let exs = [(env', ex')];
            constrainExp(exp, exs)
        }
        | (Ector(id1, ex'), Rctor(id2, r')) when id1 == id2 => unevaluate(r', ex')
        | (_, Rictor(id, r')) => unevaluate(r', Ector(id, ex))
        | (_, Rcase(r', branches, env)) => {
            let cons = List.map(
                ((ctor_id, (id, e1))) => {
                    let k1 = unevaluate(r', Ector(ctor_id, Top));
                    let k2 = constrainExp(e1, [([(id, Rictor(ctor_id, r')), ...env], ex)]);
                    switch (mergeCons(k1, k2)) {
                        | None => None
                        | x => x
                        }
                },
                branches) |> List.filter(optionPred);
            switch (cons) {
                | [] => None
                | [k, ...xs] => k
                }
        }
        // When none of the inference rules apply
        | _ => None // fail
    }
}

// exs = list((env, ex))
// e -> r using env
// unevaluate(r, ex)

and constrainExp = (exp, exs) => {
    switch (exs) {
        | [] => Some(([], []))
        | [(env, ex), ...xs] => {
            switch (constrainExp(exp, xs), unevaluate(Evaluator.eval(env, exp), ex)) {
                | (None, _) => None
                | (_, None) => None
                | (Some((k1, _)), Some((k2, _))) => Some((List.concat([k1, k2]), []))
                }
        }
    }
}

and mergeCons = (k1, k2) => {
    switch (k1, k2) {
        | (None, _) => None
        | (_, None) => None
        | (Some((u1, f1)), Some((u2, f2))) => 
            Some((List.concat([u1, u2]), List.concat([f1, f2])))
    }
}
    
and optionPred = (x) =>
    switch(x) {
        | Some(_) => true
        | None => false
        };



