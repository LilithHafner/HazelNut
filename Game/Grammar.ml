type id = Id.t
type hole = id
type variable = id

type exp =
    | Hole of hole
    | Variable of variable
    | Application of exp * exp
    | Lambda of variable * annotation * exp
and annotation = (exp * exp) list
type env = (variable, qexp) Map.t
and qexp = QExp of env * exp * tail
and tail = qexp list
type assertion = qexp * qexp