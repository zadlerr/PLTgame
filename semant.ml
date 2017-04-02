open Ast

module StringMap = Map.Make(String)

let check (globals, functions) = 

  (* same as microC, sorts a given list to see if there are multiple entires of      same keyword *)
  let report_duplicates exceptf list = 
    let rec helper = function 
	 n1 :: n2 :: _ when n1 = n2 -> raise(Failure (exceptf n1))
	| _ :: t -> helper t
	| [] -> ()
    in helper (List.sort compare list)
  in 

 (* used for checking to see if a variable has type void, returns unit if not*) 
  let check_not_void exceptf = function 
	(Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in 

  let check_assign lvaluet rvaluet err =
    (* types are equal*) 
    (* implement op == *)
    if lvaluet == rvaluet then lvaluet else raise err 
  in 

  (* CHECKING GLOBAL VARIABLES *)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  report_duplicates (fun n -> "duplicate global " ^ n) (List.map snd globals);
