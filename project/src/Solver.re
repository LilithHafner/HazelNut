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
        | [] => (f, hContext)
        | [(h, x), ...us] => {
            // IMPORTANT
            // Change this to first check if a filling is contained in the set of fillings.

            let (context, t) = Tools.lookup(h, hContext);
            let (k', hContext') = Filler.fill(hContext, f, context, h, t, x);
            let (us', f') = k';
            let k'' =  (us' @ us, f');
            solve_h(hContext', k'');
        }
    }
};

let solve = (k) => {
    let Some(k') = k;
    let (u, _) = k';
    let hContext = Typing.generateHoleContextU(u);
    solve_h(hContext, k')
};
