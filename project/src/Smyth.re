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
bidirectional) type inference for all expressions.*/
type example_refined_typed_exp = (example_refined_type, exp)
and exp = 
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Cons(example_refined_typed_exp, example_refined_typed_exp)
  | Nil
  | Function(identifier, example_refined_typed_exp)
  | Application(example_refined_typed_exp, example_refined_typed_exp)
  | Hole(hole_identifier)

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
If an expression must satisfy two types, examples, or ERTs 
(example refined types), then this set of functions is appropriate. 
They take in two such constraints, and return a stritcter constraint
that is equivilant to the satisfaction of both input constraints.
*/
let type_intersection (t1:type_, t2:type_): type_ = 
  switch (t1, t2) {
  | (Any_t, x) | (x, Any_t) => x
  | (a,b) when a == b => a
  | _ => Fail_t
  }
let merge_examples = List.append
let example_refined_type_intersection (ert1:example_refined_type, ert2:example_refined_type):example_refined_type =
  switch (ert1, ert2) {
  | ((t1, es1), (t2, es2)) => (type_intersection(t1,t2), merge_examples(es1, es2))
  }


/*
This is where the magic happens.

You call this function with an example_refined_typed_exp, it propigates 
top-level ERTs (example refined types) down one level, recurs, and then 
propigates ERTs back up one level, returning the result.

There is an option to call a function call a leaf, and leave it at that,
as an ordinary type checker would do, or to continue through, as an
ordinary evaluator would do. This will continue through, albiet at the
risk of nontermination in the presence of nonterminating programs.
*/
let bidirectional_typecheck (sketch:example_refined_typed_exp, environment:environment):example_refined_typed_exp =
    sketch


/*
This is a sanity check to verify my IDE 
(and provide a syntax reference for printing).
*/
let add (x:int, y:int):int = x+y

print_int(add(11, 7));
print_string("\n");

