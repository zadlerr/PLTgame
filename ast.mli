type typ = Int | String | Char | Bool | Void 

type bind = typ * string


type expr =
	  Int_Literal of int
	| String_Literal of string
	| Char_Literal of char
	| Id of string
	| Assign of string * expr
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
