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

open Draft_of_take_on_Smyth;

type ex =
    | Top 
    | Eunit 
    | Epair(ex, ex)
    | Efunc(value, ex)

and value =
    | Vunit 
    | Vpair(value, value);

// Generates constraints from a result and example
let rec unevaluate = (res, ex) => {
    switch ((ex, res)) {
        | (Top, _) => []
        | (Eunit, Runit) => []
        | (Epair(ex1, ex2), Rpair(r1, r2)) => List.concat([unevaluate(r1, ex1), unevaluate(r2, ex2)])
        | (Efunc(v, ex'), Rapp(r1, r2)) => [] 
        | _ => [] 
    }
};
