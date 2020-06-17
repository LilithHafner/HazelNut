/*


/*
Ert means Example Refined Type. Where an ordinary type 
inference system uses types, this uses erts.

In a flexibly guided solver model, all holes, even implied
via function call, should be visible and constrainable.
*/

type identifier = string
type hole_identifier = int

type ert_exp = (ert, exp)
and ert_value = (ert, value)
/* Minimal primitives for faster development. 
Backfilling primitives should be easy. */
and exp = 
  | Bool(bool) /* Bools don't need explicit erts. When you want 
                the ert of a bool, switch on it's value. 
                    True => (Bool_t, [(whatever_context_you_care_about -> True)*])
                    False => (Bool_t, [(whatever_context_you_care_about -> True)*])
                */
  | Variable(identifier)/* Variables don't need explicit erts. When you want the ert 
                        of a variable, look it up (you should always be able to).
                        All values have erts. */
  | Function(ert, identifier, exp)
  | Application(ert, exp)
  | Hole(hole_identifier) /* Hole erts are managed seperately, in a mapping from 
                        Hole identifier to ert, that way all constraints on a hole
                        apply at all times. */
/* these are called values, not results, because 
this system uses the theory of type inference.
A value is a fully evaluated expression. */
and value =  
  | Bool(bool)
  | Function(identifier, ert_exp)
  | Application(Hole(hole_identifier), ert_value)
  | Hole(hole_identifier)
and type_ =
  | Bool_t
  | Function_t(type_, type_)
  | Any_t
  | Fail_t

and environment = list((identifier, exp))
and example = (environment, exp)
and examples = list(example)
and example_refined_type = (type_, examples)
type hole_constraints = list((hole_identifier, example_refined_type))

*/