open Grammar

let application (env:env) (variable:variable) (annotation:(exp * exp) list) (body:exp) (applicand:qexp) (tail:tail):qexp * assertion list =
    let env = Map.add env variable applicand in
    QExp(env, body, tail), List.map
        (fun (e1, e2) -> QExp(env, e1, []), QExp(env, e2, []))
        annotation

let function_congruence ((QExp(env1, exp1, tail1), QExp(env2, exp2, tail2)):assertion):assertion =
    (* Note: as used by section 4, tail1 should always be empty *)
    let x = QExp(Map.empty, Variable(Id.unique ()), []) in
    QExp(env1, exp1, tail1 @ [x]), QExp(env2, exp2, tail2 @ [x])
    
let substitution:variable -> env -> qexp = Map.find

let unbound_variable_congruence: (variable -> variable -> bool) = (==)

let rec decomposition (t1:tail) (t2:tail):assertion list option = 
    match t1, t2 with
    | [], [] -> Some []
    | q1::t1, q2::t2 ->
        (match decomposition t1 t2 with
        | None -> None
        | Some rest -> Some ((q1, q2)::rest))
    | _ -> None

let context_passing (env:env) (e1:exp) (e2:exp):qexp =
    QExp(env, e1, [QExp(env, e2, [])])
    
(* Hole Filling implemented implicitly *)
(* Hole Consistency implemented implicitly *)
(* Reflexivity implemented implicitly *)
(* Symmetry implemented implicitly *)
(* Transitivity implemented implicitly *)

let refinement (env:env) (hole:hole) (tail:tail) (q2:qexp): (exp * assertion list) list =
    let exp = Fresh.lambda hole in
    [exp, [QExp(env, exp, tail), q2]]
    
let application_filling (env:env) (hole:hole) (q2:qexp):(exp * assertion list) list =
    let exp = Fresh.application hole in
    [exp, [QExp(env, exp, []), q2]]

let bound_variable_filling (env:env) (q2:qexp) (rest:(exp * assertion list) list): (exp * assertion list) list =
    List.fold_left
        (fun rest (variable, value) -> (Variable(variable), [(value, q2)])::rest)
        (* TODO: switch to a typed map *)
        rest
        (Map.entries env)

let rec constructor_filling (env:env) (hole:hole) (variable:variable) (tail:tail) (rest:(exp * assertion list) list):(exp * assertion list) list =
    (* TODO: refactor to use decomposition *)
    match tail with 
    | [] -> (Variable(variable), [])::rest
    | q2::[] -> 
        let hole = Hole(Id.tag Applicator hole) in
        (Application(Variable(variable), hole), [(QExp(env, hole, []), q2)])::rest
    | q2::tail -> 
        match constructor_filling env hole variable tail rest with
            | (exp, (QExp(_, Hole(hole), []), _)::[])::rest ->
                let hole = Hole(Id.tag Applicator hole) in
                (Application(exp, hole), [(QExp(env, hole, []), q2)])::rest
            | _ -> failwith "Typo in constructor_filling"
