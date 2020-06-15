open Draft_of_take_on_Smyth
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
