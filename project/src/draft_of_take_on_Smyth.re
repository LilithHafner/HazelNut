/*
This is a hot take on Smyth. It is intended to do the same stuff, but 
slightly refactored to more directly follow from standard type-checking
and evaluation for brevity and clarity. Notably, this is a mild departure
from the ICFP paper's description, but should be semantically equivalent.
*/
type identifier = string
type hole_identifier = int

/* The reason I use these, that is, include example refined types at every node,
is that it allows more fluid annotation than assert statements. This supports
example refined typed annotations for all expressions, much like how Reason
supports plain type annotations for all expressions.  

Also, now instead of treating holes as special, we can use standard (albiet
bidirectional) type inference for all expressions. */
type example_refined_typed_exp = (example_refined_type, exp)
and exp = 
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Cons(example_refined_typed_exp, example_refined_typed_exp)
  | Nil
  | Variable(identifier)
  | Function(identifier, example_refined_typed_exp)
  | Application(example_refined_typed_exp, example_refined_typed_exp)
  | Hole(hole_identifier)
/* /* result could be called value. It is an exp that cannot be evaluated any more. */
and result = 
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Cons(example_refined_typed_exp, example_refined_typed_exp)
  | Nil
  | Variable(identifier)
  | Function(identifier, example_refined_typed_exp)
  | Application(Hole(hole_identifier), example_refined_typed_exp)
  /* This could be problematic because it could potentially be evaluated further, 
  as the disctinction between free and bound variables is not accessable here.
  These types are not quite sound in what they claim. */
  | Application(Variable(identifier), example_refined_typed_exp)
  | Hole(hole_identifier)*/ 
and type_ =
  | Int_t
  | Bool_t
  | Cons_t(type_)
  | Function_t(type_, type_)
  | Any_t
  | Fail_t

and environment = list((identifier, exp))
and example = (environment, exp)
and examples = list(example)
and example_refined_type = (type_, examples)
type hole_constraints = list((hole_identifier, example_refined_type))

/*
This is where some magic happens.

You call this function with an example_refined_typed_exp, it propigates 
top-level ERTs (example refined types) down one level, recurs, and then 
propigates ERTs back up one level, returning the result.

There is an option to call a function call a leaf, and leave it at that,
as an ordinary type checker would do, or to continue through, as an
ordinary evaluator would do. This will continue through, albiet at the
risk of nontermination in the presence of nonterminating programs.
*/
let bidirectional_typecheck (sketch:example_refined_typed_exp, environment:environment):example_refined_typed_exp =
  switch(sketch) {
  | (example_refined_type, exp) => (example_refined_type, switch(exp) {
    | Variable(identifier) => Environment.lookup(environment, identifier)
    | x => x
    })
  }


/*
This should return a sequence, but I don't know the syntax for that in reason.
*/
let refine (specification:example_refined_type, environment:environment):list(example_refined_typed_exp) = 
  switch(specification) {
  | (Int_t, [(env, Int(1))]) when env == environment => [((Int_t, []), Int(1))]
  | _ => []
  }


/*
This is the main function, and could be called main just as well as synthesize.
It takes in a sketch, including any ERT (example refined type) annotations, and
attempts to synthesize a program without holes. It has three possible outcomes:
In the vernacular of SAT/SMT: UNSAT, and SAT, UNSAT, and TIMEOUT. UNSAT should
only happen if there are inconsistant type annotations (implied or explicit).
*/
let synthesize (sketch:example_refined_typed_exp):example_refined_typed_exp = 
  sketch

/*
This is a sanity check to verify my IDE 
(and provide a syntax reference for printing).
*/
let add (x:int, y:int):int = x+y

print_int(add(11, 7));
print_string("\n");

