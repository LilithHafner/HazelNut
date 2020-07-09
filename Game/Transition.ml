open Grammar
open InferenceRules
open Up
open Pp
open Printf

type transition =
    | Unlinked of assertion list list
    | Linked of hole * (exp * assertion list) list

(* TODO*: apply symmetry using the regular expression replacement "(\h+)\| (.+), q2 ->" -> "\1| \2, q2 \n\1| q2, \2 ->" (undo with "(\h+)\| (.+), q2 \n\1\| q2, \2 ->" -> "\1| \2, q2 ->") and manually duplicate the bound variable match *)
let apply_inference_rules (assertion:assertion):transition =
    if Parameters.verbosity > 1 then printf "Assertion in: \n%s\n" (string_of_assertion assertion);
    let out = (
    match assertion with
    | QExp(env, Application(exp1, exp2), tail), q2 
    | q2, QExp(env, Application(exp1, exp2), tail) ->
        (* TODO?: factor out application_passing *)
        Unlinked [[QExp(env, exp1, QExp(env, exp2, [])::tail), q2]]
    
    | QExp(env, Variable(variable), tail), q2 when Map.mem variable env ->
        (* TODO: This is O(the number depth of application of the variable's binding) because of the list append. Perhaps it can be O(1)? *)
        let QExp(env, exp, inner_tail) = substitution variable env in
        Unlinked [[QExp(env, exp, inner_tail @ tail), q2]]
    | q2, QExp(env, Variable(variable), tail) when Map.mem variable env ->
        (* TODO: This is O(the number depth of application of the variable's binding) because of the list append. Perhaps it can be O(1)? *)
        let QExp(env, exp, inner_tail) = substitution variable env in
        Unlinked [[QExp(env, exp, inner_tail @ tail), q2]]
    
    | QExp(env, Lambda(variable, annotation, body), applicand::tail), q2 
    | q2, QExp(env, Lambda(variable, annotation, body), applicand::tail) ->
        let q1, rest = application env variable annotation body applicand tail in
        Unlinked [(q1,q2)::rest]
    
    | QExp(env, Hole(hole), applicand::tail), q2 
    | q2, QExp(env, Hole(hole), applicand::tail) ->
        Linked(hole, refinement env hole (applicand::tail) q2)
        
    | QExp(env, Hole(hole), []), q2 
    | q2, QExp(env, Hole(hole), []) -> 
        Linked(hole, match q2 with
        
        | QExp(env2, Lambda(variable, assertions, body), []) ->
            refinement env hole [] q2
        
        | QExp(env2, Hole(hole2), []) ->
            (* TODO?: allow application on either side of the congruence *)
            bound_variable_filling env q2 @@
            application_filling env hole q2
        
        | QExp(env2, Variable(variable), tail) ->
            bound_variable_filling env q2 @@
            (if Id.protected variable 
                then (fun x -> x) 
                else constructor_filling env hole variable tail) @@
            application_filling env hole q2

        | _ -> failwith "Typo in apply_inference_rules")
    
    | QExp(env, Lambda(variable, annotation, body), []), q2 
    | q2, QExp(env, Lambda(variable, annotation, body), []) ->
        Unlinked [[function_congruence assertion]]
        
    | QExp(_, Variable(variable1), tail1), QExp(_, Variable(variable2), tail2) when variable1 == variable2 ->
        Unlinked (match decomposition tail1 tail2 with
            | None -> []
            | Some assertions -> [assertions])
            
    | QExp(_, Variable(variable1), _), QExp(_, Variable(variable2), _) ->
        Unlinked []
    ) in
    if Parameters.verbosity > 1 then (match out with 
    | Unlinked(assertionss) -> printf "Assertionss out: \n%s\n" 
        (string_of_list (string_of_list string_of_assertion) assertionss);
    | Linked(hole, assertionss) -> printf "Linked assertionss out (hole=%s): \n%s\n"
        (string_of_id hole)
        (string_of_list 
            (fun (exp, assertions) -> 
                string_of_exp exp^" -> "
                ^string_of_list string_of_assertion assertions) 
            assertionss)
    );
    out