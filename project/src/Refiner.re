// Holds the refine function

open Types;

let refinable = (typ, exs) => 
    switch (typ) {
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => true
        | D(adt) => {
            switch (exs) {
                | [] => true
                | [(env, Ector(id, adt, _)), ...xs] => 
                    List.length(List.filter(((_, Ector(id', adt, _))) => id == id', xs)) == List.length(xs)
                | _ => false
            }
        }
        | _ => false
        };

let rec allUnit = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, Eunit), ...xs] => allUnit(xs)
        | _ => false
    }
};

let rec allPairs = (exs) => {
    Js.log("Pair check");
    switch (exs) {
        | [] => true
        | [(_, Epair(_, _)), ...xs] => allPairs(xs)
        | _ => false
    }
};

let rec allFuncs = (exs) => {
    Js.log("Func check");
    switch (exs) {
        | [] => true
        | [(_, Efunc(_, _)), ...xs] => allFuncs(xs)
        | _ => false
    }
};

let allConstructs = (exs):option(int) => {
    Js.log("Con check");
    let c = switch (exs) {
        | [] => None
        | [(_, Ector(id, _, ex)), ...xs] => Some(id)
        | _ => None
        };
    switch (c) {
        | None => None
        | Some(i) => {
            let haveIdC = List.filter(
                ((env, ex)) => {
                    switch(ex) {
                        | Ector(i, _, _) => true
                        | _ => false
                        }
                },
                exs);
            if (List.length(haveIdC) == List.length(exs)){
                c
            } else {
                None
            }
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

let prepConsExs = (exs) => List.map(
    ((env, Ector(id, _, ex))) => (env, ex),
    exs);

// Takes in hole context, context, goal type, and example constraints
// To Do:
//   - Need to generate hole and variable identifiers. (misc different file)
//   - Figure out how to suppress warnings
//   - Implement handling hole contexts (unclear in paper how this is handled)
// (Delta, Gamma, Type, X) -> e, G


// Testing::
//   goal type = (context, hole id, type, exs)
//   What it does: match against type.
//   type = ??1 : t1 -> t2 => (\x:t1  => ??2: t2)

let refine = (context, typ, exs) => {
    Js.log(Printer.string_of_type_(typ));
    switch (typ) {
        | Unit_t when allUnit(exs) => (Unit, [])
        | Pair_t(t1, t2) when allPairs(exs) => {
            let x = IdGenerator.getId();
            let y = IdGenerator.getId();
            (Pair(Hole(x), Hole(y)), [(context, x, t1, firstExs(exs)), (context, y, t2, sndExs(exs))])
        }
        | Function_t(t1, t2) when allFuncs(exs) => {
            let x = IdGenerator.getId();
            let h = IdGenerator.getId();
            (Function(x, t1, Hole(h)), [([(x, t1), ...context], h, t2, prepFuncExs(exs, x))])
        }
        | D(adt) => {
            let c = allConstructs(exs);
            switch (c) {
                | Some(i) => {
                    let h = IdGenerator.getId();
                    let t = Tools.lookup(adt, Types.sigma) |> Tools.lookup(i);
                    (Ctor(i, adt, Hole(h)), [(context, h, t, prepConsExs(exs))]) 
                }
                | None => failwith("Examples inconsistent with constructor")
            }
        }
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => {
            Js.log(Printer.string_of_type_(typ));
            Js.log(Printer.string_of_excons(exs));
            failwith("Goal type inconsistent with examples")
        }
        | _ => failwith("Not a refinement type")
        }
};

