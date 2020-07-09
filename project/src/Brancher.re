
open Types;

let simplifyConstructor = (res) =>
    switch (res) {
        | Rictor(id1, _, Rctor(id2, _, r)) when id1 == id2 => r
        | _ => res
        };

let rec branch = (delta:hole_context, gamma:context, typ:type_, exs:excons) => {
    let datatypes = List.map(
        ((id, t)) => t,
        gamma)
        |> List.filter(
            (t) => switch (t) {
                | D(_) => true
                | _ => false
                },
        )
        |> List.map(
            (t) => switch (t) {
                | D(adt) => adt
                | _ => failwith("Error in branch")
                },
        )
        |> List.sort_uniq(
            (t1, t2) => 0,
        );

    List.map(
        (d) => branch_indiv(delta, gamma, typ, exs, d),
        datatypes)
}

and branch_indiv = (delta, gamma, typ, exs, datatype) => {
    let es = Guesser.guess(delta, gamma, D(datatype), 1);
    List.map(
        (e) => {
            let constructors = Tools.lookup(datatype, sigma);
            let distributedExs = distribute(delta, exs, datatype, e, constructors);
            let unevalCons: option(unevalcons) = List.map(
                (exs) => Unevaluator.constrainExp(delta, e, exs),
                distributedExs)
                |> List.fold_left(Unevaluator.mergeCons, Some(([], [])));
                let branches = List.map(
                    ((id, _)) => {
                        let x = IdGenerator.getId();
                        let h = IdGenerator.getId();
                        (id, (x, Hole(h)))
                    },
                    constructors);
                let exp = Case(e, branches);
                let newExCons = List.map2(
                    (dExs, (id, (x, _))) => List.map(
                        ((env, ex)) => {
                            let r = simplifyConstructor(Rictor(id, datatype, Evaluator.eval(env, e)));
                            ([(x, r), ...env], ex)
                        },
                        dExs),
                    distributedExs, branches);

                let goals = List.mapi(
                    (i, (id, (var, Hole(h)))) => {
                        let (_, ti) = List.nth(constructors, i);
                        let xs = List.nth(newExCons, i);
                        ([(var, ti), ...gamma], h, typ, xs)
                    },
                    branches);
                (exp, goals, unevalCons)
        }, es);
}

and distribute = (delta, exs, adt, scrut, ctors) => {
    List.map(
        ((id, t)) => {
            List.filter(
                ((env, ex)) => {
                    let r = Evaluator.eval(env, scrut);
                    Unevaluator.unevaluate(delta, r, Ector(id, adt, Top))
                        |> Unevaluator.optionPred
                }, exs)
        }, ctors)
};



