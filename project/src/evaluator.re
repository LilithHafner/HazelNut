open Types;

// Takes an expression and returns a corresponding result by
// Evaluating the expression until a hole is in applciation position
// This process is completely deterministic

// Take in an environment E and a expression e, and change the expression to
// a result. Evaluator.eval(E, e) 
// e = () => r = ()
// e = (\x.x) () => r = ()
// e = (\x.??) () => (E, x -> ()) ??
// e = 6 + 11 => 17
// e = 6 + ?? => 6 + [E] ??

let rec eval = (_env:environment, e:exp):res => {
    switch (e) {
        | Hole(x) => Rhole(x, _env)
        | Var(x) => Tools.lookup(x, _env)
        | Function(name, id, typ, exp) => Rfunc(name, id, typ, exp, _env)
        | Application(e1, e2) => {
            let r1 = eval(_env, e1);
            let r2 = eval(_env, e2);
            switch (r1) {
                | Rfunc(n, id, _, exp, env) => eval([(n, r1), (id, r2), ...env], exp)
                | _ => Rapp(r1, r2)//This line seems fishy to me.
            }
        }
        | Unit => Runit 
        | Pair(e1, e2) => Rpair(eval(_env, e1), eval(_env, e2))
        | Fst(e1) => Rfst(eval(_env, e1))
        | Snd(e1) => Rsnd(eval(_env, e1))
        | Int(x) => Rint(x)
        | Float(f) => Rfloat(f)
        | Bool(b) => Rbool(b)
        | Cons(e1, e2) => Rcons(eval(_env, e1), eval(_env, e2))
        | Nil => Rnil 
        | Ctor(id, adt, e1) => Rctor(id, adt, eval(_env, e1))
        // Need to come back and handle indeterminate case eventually.
        | Case(e1, branches) => {
            Js.log("evaluator case");
            Js.log(Printer.string_of_exp(e1));
            Js.log(Printer.string_of_res(eval(_env, e1)));
            switch (eval(_env, e1)) {
                | Rctor(id, _, r) => {
                    let (pat, e2) = Tools.lookup(id, branches);
                    eval(getPatEnv(pat, r) @ _env, e2)
                }
                | _ => failwith("Type error: expected a constructor within case")
            }
        }
    }
}

and evalAndFill = (env, e, f) => eval(env, e) -> fillRes(f) 

and fillEnv = (env, f) => List.map(
        ((id, r')) => (id, fillRes(r', f)), env)

and fillRes = (r, f): res => {
    switch (r) {
        | Rhole(x, env) => 
            try(evalAndFill(env, fillExp(Tools.lookup(x, f), f), f)) {
                | Not_found => Rhole(x, fillEnv(env, f))
                }
        | Rfunc(n, x, t, e, env) => Rfunc(n, x, t, fillExp(e, f), fillEnv(env, f))
        | Rapp(r1, r2) => Rapp(fillRes(r1, f), fillRes(r2, f))
        | Rpair(r1, r2) => Rpair(fillRes(r1, f), fillRes(r2, f))
        | Rfst(r1) => Rfst(fillRes(r1, f))
        | Rsnd(r1) => Rsnd(fillRes(r1, f))
        | Rctor(id, d, r1) => Rctor(id, d, fillRes(r1, f))
        | Rictor(id, d, r1) => Rictor(id, d, fillRes(r1, f))
        | Rcase(r1, bs, env) => Rcase(fillRes(r1, f), 
            List.map(
                ((id, (pat, e))) => (id, (pat, fillExp(e, f))), bs),
            fillEnv(env, f))
        | x => x
    }
}


and fillExp = (exp, f) => {
    switch (exp) {
        | Hole(x) => {
            try(fillExp(Tools.lookup(x, f), f)) {
                | Not_found => Hole(x)
                }
        }
        | Var(x) => Var(x)
        | Function(name, id, typ, e) => Function(name, id, typ, fillExp(e, f))
        | Application(e1, e2) => Application(fillExp(e1, f), fillExp(e2, f))
        | Unit => Unit 
        | Pair(e1, e2) => Pair(fillExp(e1, f), fillExp(e2, f))
        | Fst(e1) => Fst(fillExp(e1, f))
        | Snd(e1) => Snd(fillExp(e1, f))
        | Int(x) => Int(x)
        | Float(f) => Float(f)
        | Bool(b) => Bool(b)
        | Ctor(id, adt, e) => Ctor(id, adt, fillExp(e, f))
        | Case(e1, branches) => Case(fillExp(e1, f), List.map(((id, (pat, e))) => (id, (pat, fillExp(e, f))), branches))
    }
}

and getPatEnv = (pat, r) => 
    switch (pat, r) {
        | (V(x), _) => [(x, r)]
        | (P(p1, p2), Rpair(r1, r2)) => getPatEnv(p1, r1) @ getPatEnv(p2, r2)
        | _ => failwith("Result does not match constructor pattern")
        }

