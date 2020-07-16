// Houses the main recursive function which iterates through
// the list of all unsolved holes and fills them.
// Solver:: hContext, (U, K)
//   -> Calls Filler (hContext, f, context, h, typ, excons)
// Problem: context isn't correct.
// U = list h -> list (env, ex)
// F = list h -> e

// 1 gamma i;
// We need way to generate the hole context

let rec solve_h = (hContext, k) => {
    let (u, f) = k;
    switch (u) {
        | [] => Some((f, hContext))
        | [(h, x), ...us] => {
            // IMPORTANT
            // Change this to first check if a filling is contained in the set of fillings.

            // Note, will change fill to return a list of contexts and constraints, then use map. Then
            // check if any succeed, so we should probably change it to some / none

            let (context, t) = Tools.lookup(h, hContext);
            let ks = Filler.fill(hContext, f, context, h, t, x);
            let candidates = List.map(
                ((k', hContext')) => { 
                    let (us', f') = k';
                    let k'' =  (us' @ us, f');
                    solve_h(hContext', k'');
                }, ks);
            switch (List.filter(Filler.optionPred, candidates)) {
                | [] => None
                | [x, ...xs] => x
                }

        }
    }
};

let solve = (k, e) => {
    Refiner.outFunc := true;
    let Some(k') = k;
    let (u, _) = k';
    let hContext = Typing.generateHoleContextU(u);
    switch (solve_h(hContext, k')) {
        | None => failwith("Could not synthesize expression that met constraints")
        | Some((f, delta)) => {
            Js.log("Expression:");
            Js.log(Printer.string_of_exp(Evaluator.fillExp(e, f)));
            Js.log("Hole fillings:");
            (f, delta)
        }
    }
};
