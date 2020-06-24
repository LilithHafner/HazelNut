// Houses the main recursive function which iterates through
// the list of all unsolved holes and fills them.

let rec solve_h = (hContext, k) => {
    let (u, f) = k;
    switch (u) {
        | [] => (f, hContext)
        | [(h, x), ...us] => {
            let (context, t) = Tools.lookup(h, hContext);
            let (k', hContext') = Filler.fill(hContext, f, context, h, t, x);
            let (us', f') = k';
            let k'' =  (us' @ us, f');
            solve_h(hContext', k'');
        }
    }
};

let solve = (hContext, k) => {
    let Some(k') = k;
    solve_h(hContext, k')
};
