// File to hold the main guessing function

// Takes in context and hole type and returns an expression.
// (context, goal type) -> e (all of the goal type)

// Gamma [(variable name, type)]
// guess(context, t) ::> 

open Types;

let memo = Array.make(10, []);

let rec partition_h = (n, m, i) => {
    if (m == i) {
        []
    } else {
        [(n, m), ...partition_h(n - 1, m + 1, i)]
    }
};

let partition = (n) => partition_h(n - 1, 1, n);

let guessApp = (delta, gamma: context, typ: type_, i: int, j: int): list(exp) => {
    let funcs = List.filter(
        (e) => switch(Typing.getType(delta, gamma, e)) {
            | Function_t(_, _) => true
            | _ => false
            },
        memo[i - 1]);
    let args = List.map(
        (e) => {
            let candidates = List.filter(
                (x) => {
                    let Function_t(t1, t2) = Typing.getType(delta, gamma, e);
                    let t = Typing.getType(delta, gamma, x);
                    t == t1
                },
                memo[j - 1]);
            List.map(
                (x) => Application(e, x),
                candidates)
        },
        funcs);
    List.concat(args)
};

let guess = (delta: hole_context, gamma: context, typ: type_, i: int): list(exp) => {
    if (i == 1) {
        let terms = List.filter(((_, t)) => t == typ, gamma);
        Js.log(Printer.string_of_context(gamma));
        Js.log(List.length(gamma));
        memo[0] = List.map(((x, _)) => Var(x), gamma);
        Js.log(List.length(memo[0]));
        List.map(((x, _)) => Var(x), terms)
    } else {
        let pairs = partition(i);
        memo[i - 1] = List.map(((n, m)) => guessApp(delta, gamma, typ, n, m), pairs) |> List.concat;
        Js.log(List.length(memo[i-1]));
        List.filter(
            (e) => Typing.getType(delta, gamma, e) == typ,
            memo[i - 1])
    }
};


