// File to hold the main guessing function

// Takes in context and hole type and returns an expression.
// (context, type) -> e

open Types;

let memo = Array.make(10, []);

let rec partition_h = (n, m) => {
    if (n <= m) {
        []
    } else {
        [(n, m), ...partition_h(n - 1, m + 1)]
    }
};

let partition = (n) => partition_h(n - 1, 1);

let guessApp = (gamma: context, typ: type_, i: int, j: int): list(exp) => {
    let funcs = List.filter(
        (e) => switch(Typing.getType(e)) {
            | Function_t(_, typ) => true
            | _ => false
            },
        memo[i]);
    let args = List.filter(
        (e) => List.exists(
            (x) => {
                let t = Typing.getType(e);
                switch(Typing.getType(x)) {
                    | Function_t(t, _) => true
                    | _ => false
                    }
            },
            funcs),
        memo[j]);
    let exps = List.concat(List.map(
        (e) => switch(Typing.getType(e)) {
            | Function_t(t1, _) => {
                let corrArgs = List.filter(
                    (e2) => t1 == Typing.getType(e2), 
                    args);
                List.map(
                    (e2) => Application(e, e2), 
                    corrArgs)
            }
            | _ => failwith("Something is wrong with guesser")
            },
        funcs));
    exps
};

let guess = (gamma: context, typ: type_, i: int): list(exp) => {
    if (i == 1) {
        let terms = List.filter(((x, t)) => t == typ, gamma);
        memo[0] = List.map(((x, _)) => Var(x), terms);
        memo[0]
    } else {
        let pairs = partition(i);
        memo[i] = List.map(((n, m)) => guessApp(gamma, typ, n, m), pairs)
            -> List.concat;
        memo[i]
    }
};


