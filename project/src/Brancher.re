
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
    let e = List.hd(Guesser.guess(delta, gamma, D(datatype), 1));
    let constructors = Tools.lookup(datatype, sigma);
    let results = List.map(
        ((env, _)) => Evaluator.eval(env, e),
        exs);
    let unevalExs = List.map(
        ((env, _)) => List.map(
            ((id, _)) => (env, Ector(id, datatype, Top)),
            constructors),
        exs) |> List.concat;
    let unevalCons: option(unevalcons) = Unevaluator.constrainExp(delta, e, unevalExs);
    let branches = List.map(
        ((id, _)) => {
            let x = IdGenerator.getId();
            let h = IdGenerator.getId();
            (id, (x, Hole(h)))
        },
        constructors);
    let exp = Case(e, branches);
    let newExCons = List.map(
        ((id, (var, _))) => 
            List.mapi(
                (j, (env, ex)) => {
                    let r = List.nth(results, j);
                    ([(var, simplifyConstructor(Rictor(id, datatype, r))), ...env], ex)
                },
                exs), 
        branches);
    let goals = List.mapi(
        (i, (id, (var, Hole(h)))) => {
            let (_, ti) = List.nth(constructors, i);
            let xs = List.nth(newExCons, i);
            ([(var, ti), ...gamma], h, typ, xs)
        },
        branches);
    (exp, goals, unevalCons)
};


