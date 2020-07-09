open Grammar
open PlayTypes

let string_of_id = Id.to_string
let parenthasized str = "("^str^")"
let rec string_of_exp = function
    | Hole(hole) -> "?"^string_of_id hole
    | Variable(variable) -> string_of_id variable
    | Application(cator, cand) -> 
            (match cator with
            | Lambda(_, _, _) -> parenthasized
            | _ -> (fun x -> x)) (string_of_exp cator)
        ^" "^
            (match cand with
            | Application(_, _) -> parenthasized
            | _ -> (fun x -> x)) (string_of_exp cand)
    | Lambda(variable, annotation, body) -> "\\"^string_of_id variable
        ^"."^string_of_annotation annotation^string_of_exp body
and string_of_annotation = function
    | [] -> "" 
    | annotation -> "{"^String.concat ", " (List.map 
        (fun (e1, e2) ->
            string_of_exp e1^" = "^string_of_exp e2)
        annotation)^"}"
and string_of_env = function
    | [] -> "·"
    | env -> "["^String.concat ", " (List.map 
        (fun (id, qexp) ->
            string_of_id id^" -> "^string_of_qexp qexp)
        env)^"]"
and string_of_qexp (QExp(env, exp, tail):qexp):string = 
    string_of_env env^string_of_exp exp^(match tail with
    | [] -> ""
    | _ -> String.concat " " (List.map 
        (fun (qexp) -> parenthasized (string_of_qexp qexp))
        tail))
let string_of_assertion ((q1, q2):assertion) = 
    (string_of_qexp q1)^" =\n"^(string_of_qexp q2)