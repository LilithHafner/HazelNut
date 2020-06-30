
open Types;

let simplifyConstructor = (res) =>
    switch (res) {
        | Rictor(id1, Rctor(id2, r)) when id1 == id2 => r
        | _ => res
        };

let branch = (delta:hole_context, gamma:context, typ:type_, exs:excons) => {
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

}

and branch_indiv = (delta, gamma, typ, exs, datatype) => {
    let e = List.hd(Guesser.guess(delta, gamma, D(datatype), 1));
    let constructors = Tools.lookup(datatype, sigma);
    let results = List.map(
        ((env, _)) => Evaluator.eval(env, e),
        exs);
    let unevalExs = List.map(
        ((env, _)) => List.map(
            ((id, _)) => (env, Ector(id, Top)),
            constructors),
        exs) |> List.concat;
    let unevalCons: option(unevalcons) = Unevaluator.constrainExp(e, unevalExs);
    let newExCons = List.map(
        ((id, _)) => {
            let x = IdGenerator.getId();
            (List.mapi(
            (j, (env, ex)) => {
                let r = List.nth(results, j);
                ([(x, simplifyConstructor(Rictor(id, r))), ...env], ex)
            },
            exs), x)
        },
        constructors);
    let holes = List.map(
        (_) => Hole(IdGenerator.getId()),
        constructors);
    let goals = List.mapi(
        (i, (xs, x)) => {
            let (_, ti) = List.nth(constructors, i);
            let Hole(h) = List.nth(holes, i);
            ([(x, ti), ...gamma], h, typ, xs)
        },
        newExCons);
    (goals, unevalCons)
};


