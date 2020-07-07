type id = Id.t
type hole = id
type variable = id

type exp =
    | Hole of hole
    | Variable of variable
    | Application of exp * exp
    | Lambda of variable * (exp * exp) list * exp
type env = (variable, qexp) Map.t
and qexp = QExp of env * exp * tail
and tail = qexp list
type assertion = qexp * qexp