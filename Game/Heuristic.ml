open Grammar
open Parameters

let rec size_of_list (size_of_element) = function
    | [] -> 1
    | x::xs -> 1 + size_of_element x + size_of_list size_of_element xs

let size_of_hole hole:int = 1
let size_of_variable variable:int = 1

let rec size_of_exp = function
    | Hole(hole) -> 1 + size_of_hole hole
    | Variable(variable) -> 1 + size_of_variable variable
    | Application(e1, e2) -> 1 + size_of_exp e1 + size_of_exp e2
    | Lambda(variable, annotation, exp) -> 
        1 + size_of_variable variable + size_of_annotation annotation + size_of_exp exp
and size_of_env env = 
    size_of_list (fun (variable, qexp) -> 
        1 + size_of_variable variable + size_of_qexp qexp) 
        env
and size_of_qexp (QExp(env, exp, tail):qexp):int = 
    1 + size_of_env env + size_of_exp exp + size_of_tail tail
and size_of_tail tail = size_of_list size_of_qexp tail
and size_of_assertion ((q1, q2):assertion):int = 
    1 + size_of_qexp q1 + size_of_qexp q2
and size_of_annotation annotation = 
    size_of_list ((fun (exp1, exp2) -> 
        1 + size_of_exp exp1 + size_of_exp exp2):(exp * exp)->int) 
        annotation


(* TODO: this is tragically and preventably slow. 
    (like Trump's response to the corronavirus)  *)
let heuristic (assertion:assertion):float option = 
    match Transition.apply_inference_rules assertion with
    | Unlinked(a) when List.length a == 1 -> None
    | _ -> Some(-.log (float_of_int (size_of_assertion assertion)))
