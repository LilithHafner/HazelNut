/*
This is a draft and a working document, intended as much to build my
understanding of Smyth, its goals, and the world it opperates in as
to replicate its functionality. It is an expiremental, not a final draft.

This is a hot take on Smyth. It is intended to do the same stuff, but 
slightly refactored to more directly follow from standard type-checking
and evaluation for brevity and clarity. Notably, this is a mild departure
from the ICFP paper's description, but should be semantically equivalent
at the highest level (albiet not neccessarrily at intermediary levels).
*/
type identifier = string
type hole_identifier = int

/* A reason to use these, that is, include example refined types at every node,
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
This is where some magic happens.

You call this function with an example_refined_typed_exp, it propigates 
top-level ERTs (example refined types) down one level, recurs, and then 
propigates ERTs back up one level, returning the result.

There is an option to call a function call a leaf, and leave it at that,
as an ordinary type checker would do, or to continue through, as an
ordinary evaluator would do. This will continue through, albiet at the
risk of nontermination in the presence of nonterminating programs.
*/
/*
Low priority: Perhaps the order of arguments should be reversed to 
allow for refactoring using partial application in recursive calls?
*/
let rec bidirectional_typecheck (ert_exp:example_refined_typed_exp, environment:environment):example_refined_typed_exp = {
  let (toplevel_ERT, exp) = ert_exp;

  let exp_but_with_ERTs_propigated_one_step:exp = 
      propigate_ERTs_down_one_level(toplevel_ERT, exp, environment);

  let exp_but_with_ERTs_propigated_all_the_way_down_and_almost_all_the_way_back_up:exp = 
      recurse(exp_but_with_ERTs_propigated_one_step, environment);

  let toplevel_ERT_implied_by_childrens_ERTs:example_refined_type = 
      propigate_ERTs_up_one_level(exp_but_with_ERTs_propigated_all_the_way_down_and_almost_all_the_way_back_up);

  let new_ERT:example_refined_type = 
      example_refined_type_intersection(toplevel_ERT, toplevel_ERT_implied_by_childrens_ERTs);
  
  (new_ERT, exp_but_with_ERTs_propigated_all_the_way_down_and_almost_all_the_way_back_up)
}
and propigate_ERTs_down_one_level (toplevel_ERT:example_refined_type, exp:exp, environment:environment):exp = 
  exp
/* switch(exp) {
  | Int(x) => Int(x)
  | Float(x) => Float(x)
  | Bool(x) => Bool(x)
  | Variable(identifier) => Environment.lookup(environment, identifier)
  | Cons(head, tail) => Match Cons(
    failwith("Unexpected type error. A cons exp was not supposed to be a list.") bidirectional_typecheck(head, environment), bidirectional_typecheck(tail, environment)
    )
  | Nil => Nil
  | Function(identifier, example_refined_typed_exp)
  | Application(example_refined_typed_exp, example_refined_typed_exp)
  | Hole(hole_identifier)
  | Variable(identifier) => Environment.lookup(environment, identifier)
  | Variable(identifier) => Environment.lookup(environment, identifier)
  | x => x 
}*/
/* This whole function can probably be replaced with a 
builtin monadic fold opperation or some such. */
and recurse (exp:exp, environment:environment):exp =
  switch(exp) {
  | Int(x) => Int(x)
  | Float(x) => Float(x)
  | Bool(x) => Bool(x)
  | Cons(head, tail) => Cons(bidirectional_typecheck(head, environment), bidirectional_typecheck(tail, environment))
  | Nil => Nil
  | Variable(id) => Variable(id)
  /* Something special may need to happen here, a merge of sorts. 
  When a function is called, we should recurse from the body such that
  the constraints on repeated and recursive calls to functions mesh properly. 
  Perhaps that happens automatically? This is a point if confusion for me. */
  | Function(id, body) => Function(id, bidirectional_typecheck(body, environment))
  | Application(cand, cator) => Application(bidirectional_typecheck(cand, environment), bidirectional_typecheck(cator, environment))
  | Hole(hole_identifier) => Hole(hole_identifier)
}
and propigate_ERTs_up_one_level(exp:exp):example_refined_type =
  (Any_t, [])

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

print_int(add(4, 7));
print_string("\n");