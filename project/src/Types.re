//----------------------------------------------------------------------
//                                Types
//----------------------------------------------------------------------

// Variable and hole names
type identifier = string
type hole_identifier = int

// Expressions in the language
//   Very small for now
type exp = 
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Cons(exp, exp)
  | Nil 
  | Variable(identifier)
  | Function(identifier, exp)
  | Application(exp, exp)
  | Hole(hole_identifier)
  | Unit 
  | Var(identifier)
  | Pair(exp, exp)
  | Fst(exp)
  | Snd(exp)

// Results in the language
//   Act as values that can have holes
and res =
    | Rint(int)
    | Rfloat(float)
    | Rbool(bool)
    | Rcons(res, res)
    | Rnil 
    | Rfunc(identifier, exp, environment)
    | Rapp(res, res)
    | Rhole(hole_identifier, environment)
    | Runit 
    | Rpair(res, res)
    | Rfst(res)
    | Rsnd(res)

// Types in the language
//   Currently not in much use
and type_ =
  | Int_t
  | Bool_t
  | Cons_t(type_)
  | Function_t(type_, type_)
  | Any_t
  | Fail_t

// Map from variable names to results
and environment = Tools.pairlist(identifier, res)
// Map from variable names to types
and context = Tools.pairlist(identifier, type_)

// Types all of the holes
type hole_constraints = list((hole_identifier, type_))

// Examples
//   Needs to be filled out more
type ex =
    | Top 
    | Eunit 
    | Epair(ex, ex)
    | Efunc(value, ex)

// Actual values
//   For now a single constant
//   plus pairs
and value =
    | Vunit 
    | Vpair(value, value);

//----------------------------------------------------------------------
//                     Typecasting Functions
//----------------------------------------------------------------------

// val -> exp
let rec valToExp = (v) => {
    switch (v) {
        | Vunit => Unit 
        | Vpair(v1, v2) => Pair(valToExp(v1), valToExp(v2))
        }
};

// res -> val
let rec resToVal = (res) => {
    switch (res) {
        | Runit => Some(Vunit)
        | Rpair(r1, r2) => 
            switch ((resToVal(r1), resToVal(r2))) {
                | (Some(x), Some(y)) => Some(Vpair(x, y))
                | _ => None
                }
        | _ => None
        }
};

// ex -> exp
let rec exToExp = (ex) => {
    switch (ex) {
        | Epair(ex1, ex2) => 
            switch ((exToExp(ex1), exToExp(ex2))) {
                | (Some(x), Some(y)) => Some(Pair(x, y))
                | _ => None
                }
        | Eunit => Some(Unit)
        | _ => None
        }
};

// res -> val possible?
let castable = (res) => 
    switch (resToVal(res)) {
        | Some(_) => true
        | None => false
        }

