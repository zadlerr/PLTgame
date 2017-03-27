type typ = Int | Bool | Void 

type bind = typ * string


type expr =
	  Literal of int
	| Id of string
	| Call of string * expr list
	| Noexpr

type stmt =
	  Block of stmt list
	| Expr of expr
	| Return of expr  


type func_decl = {
    typ : typ;
    fname: string;
    formals : bind list; 
    locals : bind list;
    body : stmt list;
}
 
type program = bind list * func_decl list

(* bind list * func_decl list *)
(*
let string_of_typ = function
    Int  -> "int"
*)
