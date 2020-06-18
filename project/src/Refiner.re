// Holds the refine function

open Types;

let refinable = ((_, _, typ, _)) => 
    switch (typ) {
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => true
        | _ => false
        };

let rec allUnit = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, ex), ...xs] => 
            switch (ex) {
                | Eunit => allUnit(xs)
                | _ => false
                }
    }
};

let rec allPairs = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, ex), ...xs] => 
            switch (ex) {
                | Epair(_, _) => allPairs(xs)
                | _ => false
                }
    }
};

let rec allFuncs = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, ex), ...xs] => 
            switch (ex) {
                | Efunc(_, _) => allFuncs(xs)
                | _ => false
                }
    }
};

let firstExs = (exs) => List.map(
    ((env, Epair(ex1, _))) => (env, ex1),
    exs);

let sndExs = (exs) => List.map(
    ((env, Epair(_, ex2))) => (env, ex2),
    exs);

let prepFuncExs = (exs, vid) => List.map(
    ((env, Efunc(v, ex))) => ([(vid, Types.valToRes(v)), ...env], ex),
     exs);

// Takes in hole context, context, goal type, and example constraints
// To Do:
//   - Need to generate hole and variable identifiers. (misc different file)
//   - Figure out how to suppress warnings
//   - Implement handling hole contexts (unclear in paper how this is handled)
// (Delta, Gamma, Type, X) -> e, G

let refine = ((context, _, typ, exs)) => {
    switch (typ) {
        | Unit_t when allUnit(exs) => (Unit, [])
        | Pair_t(t1, t2) when allPairs(exs) => (Pair(Hole(0), Hole(0)), [(context, 0, t1, firstExs(exs)), (context, 0, t2, sndExs(exs))])
        | Function_t(t1, t2) when allFuncs(exs) => (Function("f", Hole(0)), [([("f", t1), ...context], 0, t2, prepFuncExs(exs, "f"))])
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => failwith("Goal type inconsistent with examples")
        | _ => failwith("Not a refinement type")
        }
};

