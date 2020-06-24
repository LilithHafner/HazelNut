open Types;

let rec string_of_debug_construct (c:debug_construct):string =
    switch(c) {
        | Exp(x) => string_of_exp(x)
        | Environment(x) => string_of_env(x)
        | Res(x) => string_of_res(x)
        | Type_(x) => string_of_type_(x)
        | Example(x) => string_of_example(x)
        | Constraint_(x) => string_of_constraint_(x)
    }
and string_of_exp(e:exp):string =
    switch(e) {
        | Int(int) => string_of_int(int)
        | Float(float) => Js.Float.toString(float)
        | Bool(bool) => string_of_bool(bool)
        | Cons(exp, exp2) => string_of_exp(exp) ++ "::" ++ string_of_exp(exp2)
        | Nil  => "Nil"
        | Var(identifier) => string_of_identifier(identifier)
        | Function(identifier, exp) => "\\" ++ string_of_identifier(identifier) ++ "." ++ string_of_exp(exp)
        | Application(exp, exp2) => string_of_exp(exp) ++ " " ++ string_of_exp(exp2)
        | Hole(hole_identifier) => "??_"++string_of_hole_identifier(hole_identifier)
        | Unit  => "()"
        | Pair(exp, exp2) => "(" ++ string_of_exp(exp) ++ ", " ++ string_of_exp(exp2) ++ ")"
        | Fst(exp) => "fst(" ++ string_of_exp(exp) ++ ")"
        | Snd(exp) => "snd(" ++ string_of_exp(exp) ++ ")"
    }
and string_of_res(r:res):string =
    switch(r) {
        | Rint(int) => string_of_int(int)
        | Rfloat(float) => Js.Float.toString(float)
        | Rbool(bool) => string_of_bool(bool)
        | Rcons(res, res2) => string_of_res(res) ++ "::" ++ string_of_res(res2)
        | Rnil  => "Nil"
        | Rfunc(identifier, exp, environment) => "["++string_of_env(environment)++"]\\" ++ string_of_identifier(identifier) ++ "." ++ string_of_exp(exp)
        | Rapp(res, res2) => string_of_res(res) ++ " " ++ string_of_res(res2)
        | Rhole(hole_identifier, environment) => "["++string_of_env(environment)++"]??_"++string_of_hole_identifier(hole_identifier)
        | Runit  => "()"
        | Rpair(res, res2) => "(" ++ string_of_res(res) ++ ", " ++ string_of_res(res2) ++ ")"
        | Rfst(res) => "fst(" ++ string_of_res(res) ++ ")"
        | Rsnd(res) => "snd(" ++ string_of_res(res) ++ ")"
    }
and string_of_env(e:environment):string =
    switch(e) {
        | [] => "-"
        | [(identifier,res),...ms] => string_of_identifier(identifier) ++"->"++string_of_res(res)++"; "++string_of_env(ms)
    }
and string_of_identifier = string_of_int
and string_of_hole_identifier = string_of_int
and string_of_type_(t:type_):string =
    switch(t) {
        | Int_t => "Int"
        | Bool_t => "Bool"
        | Cons_t(a, b) => "Cons("++string_of_type_(a)++","++string_of_type_(b)++")"
        | Function_t(a, b) => "("++string_of_type_(a)++"->"++string_of_type_(b)++")"
        | Unit_t => "Unit"
        | Pair_t(a, b) => "("++string_of_type_(a)++", "++string_of_type_(b)++")"
        | Any_t => "Any"
        | Fail_t => "Fail"
    }
and string_of_example(_:example):string =
    "String of example not implemented"
and string_of_one_constraint_(c):string =
    switch(c) {
        | [] => "-"
        | [(environment, (hole_identifier, example)),...cs] => 
            string_of_env(environment) ++": "++string_of_hole_identifier(hole_identifier)++"->"++string_of_example(example)++"; "++string_of_one_constraint_(cs)
    }
and string_of_constraint_(c:constraint_):string =
    switch(c) {
        | None => "None"
        | Some(c) => string_of_one_constraint_(c)
    }
