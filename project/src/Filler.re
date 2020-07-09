// Class to house the filler, which calls both refinement and guessing,
// as well as deferal.

// Entirely nondeterministic. Probably need to create a bunch of tools to aid with
// this.

// gs : [(context, hole, type, ex constraints)]

open Types;

let rec updateHoleContext_h = (delta: Types.hole_context, gs: Types.goals) => {
    switch (gs) {
        | [] => delta
        | [(context, hole, typ, _), ...gs'] => {
            let xs = updateHoleContext_h(delta, gs');
            [(hole, (context, typ)), ...xs]
        }
    }
};

let updateHoleContext = (delta, h, gs) => {
    List.filter(
        ((h', _)) => h != h',
        updateHoleContext_h(delta, gs));
}

let rec updateUnfilledHoles = (gs) => 
    switch (gs) {
        | [] => []
        | [(_, h, _, exs), ...gs'] =>
            [(h, exs), ...updateUnfilledHoles(gs')]
    };

let optionPred = (x) => 
    switch (x) {
        | Some(_) => true
        | None => false
        };

let rec guessAndCheck_h = (delta, gamma, typ, exs, i) => {
    if (i > 8) {
        None
    } else {
        let es: list(Types.exp) = Guesser.guess(delta, gamma, typ, i);
        let checked = List.filter(
            (e) => {
                Unevaluator.constrainExp(delta, e, exs) -> optionPred
            },
            es);
        switch (checked) {
            | [] => guessAndCheck_h(delta, gamma, typ, exs, i + 1)
            | [e, ..._] => Some(e)
            }
    }
};

let guessAndCheck = (delta, gamma, typ, exs) => {
    guessAndCheck_h(delta, gamma, typ, exs, 1)
};

let rec allBranchesFound = (xs) => {
    switch (xs) {
        | [] => true
        | [None, ..._] => false
        | [Some(_), ...xs] => allBranchesFound(xs)
        }
};


// In returns:
//  - K = (U, F)
//  - U = the new holes added
//  - F = The existing hole fillings + 1 new filled hole
//  - delta = the existing minus the whole just filled, plus any new holes

// Note from Sam:
//
// Branching is pretty basic right now.
// All it does is guess a scrutinee of each possible type, and
// then guesses a filling for the hole in each branch. This
// has obvious drawbacks in that no refinement will happen for these
// fillings and since we don't really guess refinement types it's a bit
// rough. 

let rec fill = (delta, holeFillings, gamma, h, typ, exs) => {
    switch (fill_h(delta, holeFillings, gamma, h, typ, exs)) {
        | Some(x) => x
        | None => failwith("Filler could not find candidate for hole")
        }
}

and fill_h = (delta, holeFillings, gamma, h, typ, exs) => {
    if (Refiner.refinable(typ, exs)) {
        let (e, gs) = Refiner.refine(gamma, typ, exs);
        let f = [(h, e), ...holeFillings];
        let delta' = updateHoleContext(delta, h, gs);
        let u = updateUnfilledHoles(gs);
        let k = (u, f);
        Some((k, delta'))
    } else {
        let e = guessAndCheck(delta, gamma, typ, exs);
        switch (e) {
            | None => {
                // Branch

                let bs = Brancher.branch(delta, gamma, typ, exs)
                    |> List.map(
                        (xs) => {
                            List.map(
                                ((exp, goals, excons)) => {
                                    let es = List.map(
                                        ((gamma', h, t, xs)) => guessAndCheck(delta, gamma',  t, xs),
                                        goals);

                                    if (allBranchesFound(es)) {
                                        let Case(e', branches) = exp;
                                        let expBranches = List.mapi(
                                            (i, (c, (x, _))) => {
                                                let Some(e'') = List.nth(es, i);
                                                (c, (x, e''))
                                            },
                                            branches);
                                        Some(Case(e', expBranches))
                                    } else {
                                        None
                                    }
                                }, xs)})
                    |> List.concat;
                switch (List.filter(optionPred, bs)) {
                    | [] => None
                    | [Some(e'), ...xs] => {
                        let f = [(h, e'), ...holeFillings];
                        let delta' = List.filter(
                            ((h', _)) => h != h',
                            delta);
                        let k = ([], f);
                        Some((k, delta'))
                    }
                }
            }
            | Some(e') => {
                let f = [(h, e'), ...holeFillings];
                let delta' = List.filter(
                    ((h', _)) => h != h',
                    delta);
                let k = ([], f);
                Some((k, delta'))
            }
            | None => None
        }
    }
};
