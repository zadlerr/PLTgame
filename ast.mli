type typ = Int | Bool | Void 

type bind = typ * string


type expr =
	  Literal of int
	| Id of string
	| Noexpr

type stmt =
	  Block of stmt list
	| Expr of expr
	| Return of expr  


type func_decl = {
    typ : typ;
    fname: string;
    body : stmt list;
(*    formals : bind list;
    locals : bind list;
*)
}
 
type program = func_decl

(* bind list * func_decl list *)
(*
let string_of_typ = function
    Int  -> "int"
*)
