open Smyth;
// Returns result associated with variable x
let rec varHelper = (_env, x) => {
    switch (_env) {
        | [] => Runit
        | [(id, r), ...e] => {
            if (x == id) {
                r
            } else {
                varHelper(e, x)
            }
        }
    }
};

let rec inEnv = (env, x) => {
    switch(env) {
        | [] => false
        | [(id, _), ...e] => {
            if (x == id) {
                true
            } else {
                inEnv(e, x)
            }
        }
    }
};

// Takes an expression and returns a corresponding result
// e -> r

let rec eval = (_env, (_, e)) => {
    switch (e) {
        | Hole(x) => Rhole(x, _env)
        | Var(x) => varHelper(_env, x)
        | Function(id, exp) => Rfunc(id, exp, _env)
        | Application((t1, e1), e2) => {
            switch (e1) {
                | Function(id, exp) => eval([(id, eval(_env, e2)), ..._env], exp)
                | _ => Rapp(eval(_env, (t1, e1)), eval(_env, e2))
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
    }
};


/* // WIP
let eGuess = (context, goal, exs, k) => {
    switch context {
        Conempt => [],
        Cont(context', x, t) => {
            if t == goal {
                let e = Var(x);
                
            }
        }
    }
}
*/

