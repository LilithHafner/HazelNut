open Types;
open Parser;
open Printer;

let split(str) = {// :(
    let rec r(inp, out, live) = {
        switch(inp) {
        | [] => ([], [live, ...out], [])
        | [' ', ...inp] =>  r(inp, [live, ...out], [])
        | [c, ...inp] =>  r(inp, out, [c, ...live])
        }
    };
    let (_,out,_) = r(List.rev(explode(str)), [], [])
    List.map(implode, out)
}
let history = ref([]);
let process(inp:list(char), stack:list(debug_construct), command:string):(list(char), list(debug_construct)) = {
    history := [[(command, implode(inp), stack), ...List.hd(history^)], ...List.tl(history^)];
    switch(command) {
    | "a" =>
        let (v, inp) = parse_debug_construct(inp);
        (inp, [v,...stack])
    | "exp" =>
        let (v, inp) = parse_exp(inp);
        (inp, [Exp(v),...stack])
    | "env" =>
        let (v, inp) = parse_environment(inp);
        (inp, [Environment(v),...stack])
    | "res" =>
        let (v, inp) = parse_res(inp);
        (inp, [Res(v),...stack])
    | "type" =>
        let (v, inp) = parse_type_(inp);
        (inp, [Type_(v),...stack])
    | "ex" =>
        let (v, inp) = parse_example(inp);
        (inp, [Example(v),...stack])
    | "eval" =>
        switch(stack) {
        | [Exp(v1),...stack] =>
        switch(stack) {
        | [Environment(v0),...stack] =>
            (inp, [Res(Evaluator.eval(v0, v1)), ...stack])
        | [] => failwith("Empty stack")
        | _ => failwith("Type error")
        }
        | [] => failwith("Empty stack")
        | _ => failwith("Type error")
        }
    | "uneval" =>
        switch(stack) {
        | [Example(v1),...stack] =>
        switch(stack) {
        | [Res(v0),...stack] =>
            (inp, [Constraint_(Unevaluator.unevaluate(v0, v1)), ...stack])
        | [] => failwith("Empty stack")
        | _ => failwith("Type error")
        }
        | [] => failwith("Empty stack")
        | _ => failwith("Type error")
        }
    | _ => failwith("Unknown command: \""++command++"\"")
    }
}
let main (inp:string, commands:string) = {
    //The module or file Str can't be found.
    //Reason can't keep up with OCaml. :(
    //let commands = Str.split(Str.regexp(" +"));
    let commands = split(commands);

    let inp = explode(inp);
    history := [[],...history^]
    let (inp, stack) = List.fold_left(
        ((inp, stack), command) => process(inp, stack, command),
        (inp, []),
        commands);
    history := [[("<print_all>", implode(inp), stack), ...List.hd(history^)], ...List.tl(history^)];
    List.iter(
        (construct) => Js.log(string_of_debug_construct(construct)),
        stack
    )
    let inp = implode(inp);
    if (inp != "") {
        Js.log("Warning, leftover input: \""++inp++"\"")
    };
}
