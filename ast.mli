type typ = Int 

type bind = typ * string

type func_decl = {
    typ : typ;
    fname: string;
(*    formals : bind list;
    locals : bind list;
    body : stmt list;
*)
}
 
type program = func_decl
(* bind list * func_decl list *)
(*
let string_of_typ = function
    Int  -> "int"
*)
