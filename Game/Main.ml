open Grammar
open Printf
open Up
open Pp
open PlayTypes
open Play
open Parameters


let debug format node = match verbosity with 
| 0 -> ()
| 1|2 -> printf format (p_string_of_node node)
| _ -> printf format (p_string_of_node node^"\n"^string_of_node node)

let get_fillings () =
    Seq.fold_left 
        (fun map (id, {choice=choice;_}) -> 
            if Map.mem id map
            then map
            else Map.add map id choice) 
        Map.empty
        (Hashtbl.to_seq links)

let rec think root passes = 
    debug "Intermediary: %s\n\n" root;
    down root;
    if abs_float root.value < stop_thinking_threshold && passes > 0
    then think root (passes - 1)

type result = 
    | SAT of (hole, exp) Map.t
    | UNSAT
    | TIMEOUT of (hole, exp) Map.t

    (* TODO: link nodes properly! *)
    (* TODO: fix sign error on up propigation through link (on link reasignment) *)

let solve (passes:int) (assertions:assertion list):result =
    let root = antagonist_dead_end () in
    debug "Pre-start: %s\n\n" root;
    add_children root None (antagonist_moves assertions);
    think root passes;
    debug "Final: %s\n\n" root;
    if root.value <= -.stop_thinking_threshold 
    then UNSAT
    else if root.value >= stop_thinking_threshold 
    then SAT(get_fillings ())
    else TIMEOUT(get_fillings ())

let rec fill_holes (fillings:(hole, exp)Map.t) (exp:exp) = 
    let r = fill_holes fillings in
    match exp with
    | Hole(hole) when Map.mem hole fillings -> 
        r (Map.find hole fillings)
    | Application(e1, e2) -> 
        Application(r e1, r e2)
    | Lambda(variable, annotation, exp) -> 
        Lambda(variable, 
            (let rec f = function
            | [] -> []
            | (e1, e2)::es -> (r e1, r e2)::f es
            in f annotation),
            r exp)
    | _ -> exp

(* \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ *)
(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)
(* \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ *)
(* let Some(there_are_major_todos_left) = Some None *)
(* TODO transition function appears to be getting called in duplicate! *)
(* TODO we don't maintain a hash of old boards! No transpositions = terrible!
This means that we get exponential in places we shouldn't 
(e.g. using the assertion e=e) *)
(* Bug: ? 4 = P 4 4 generates hole fillings with wrong ids *)
(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)
(* \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ *)
(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
		while true do
			let t1 = Sys.time() in
            printf "e1>%!";
            let e1 = Parser.main Lexer.token lexbuf in
            printf "%s\ne2>%!" (string_of_exp e1);
            let e2 = Parser.main Lexer.token lexbuf in
            printf "%s\n\n" (string_of_exp e2);
            
            let assertions = (QExp(Map.empty, e1, []), QExp(Map.empty, e2, []))::[] in
            printf "Starting assertion:\n%s\n\n"  (string_of_assertion (List.nth assertions 0)); 
(*			
            let exp = Parser.main Lexer.token lexbuf in
			printf "\nInput interpretation:\n%s\n\n" (string_of_exp exp); 
            
            let assertions = (QExp(Map.empty, exp, []), QExp(Map.empty, exp, []))::[] in
            (* let assertions = (QExp(Map.empty, Variable(Id.of_string "4"), []), QExp(Map.empty, Hole(Id.of_string "1"), []))::[] in *)
            (* let assertions = (QExp(Map.empty, Variable(Id.of_string "4"), []), QExp(Map.empty, Variable(Id.of_string "5"), []))::[] in *)
            printf "Starting assertion:\n%s\n\n"  (string_of_assertion (List.nth assertions 0)); 
*)            
            let t2 = Sys.time() in
            let result = solve 100 assertions in
            let t3 = Sys.time() in
            
            (match result with 
            | SAT fillings | TIMEOUT fillings -> 
                printf "Result type:\n%s\n\n" (match result with 
                    | SAT _ -> "SAT" 
                    | TIMEOUT _ -> "TIMEOUT" 
                    | _ -> failwith "???");
                printf "Hole fillings:\n%s\n\n" (string_of_fillings fillings);
(*                let exp = fill_holes fillings exp in
                printf "Output:\n%s\n\n" (string_of_exp exp)*)
            | UNSAT ->
                printf "Result type:\nUNSAT\n\n");

			flush stdout;
            
            let t4 = Sys.time() in
            printf "Times:\n  thinking: %f\n  IO:       %f\n%!" (t3-.t2) (t2-.t1+.t4-.t3);
		done
	with Lexer.Eof ->
		exit 0