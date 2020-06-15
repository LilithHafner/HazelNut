type identifier = string
type hole_identifier = int
  
type exp = 
  | Int(int)
  | Bool(bool)
  | Cons(exp, exp)
  | Function(identifier, exp)
  | Hole(hole_identifier)

type type_ =
  | Int_t
  | Bool_t
  | Cons_t(type_)
  | Function_t(type_, type_)

type environment = list((identifier, exp))
type example = (environment, exp)
type example_refined_type = (type_, list(example))

type hole_constraints = list((hole_identifier, example_refined_type))

/* let synth (sketch:exp, specification:example_refined_type):exp =
    sketch */

let add (x:int, y:int):int = x+y

print_int(add(3, 7));
print_string("\n");

