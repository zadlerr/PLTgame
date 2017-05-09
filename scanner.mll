{ open Parser 
(*let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S!" (fun x -> x)
*)}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let int = digit+
let char = '''(ascii | digit ) '''
let string = '"' ( (ascii | escape)* as s) '"'
let id = alpha(alpha | digit | '_')*

rule token = parse
	[ ' ' '\t' '\r' '\n'] 	{ token lexbuf }(* whitespace *)
	| "##"				{ comment lexbuf }(* comment start *)	
        | '('				{ LPAREN }	(* delimiters *)
	| ')'				{ RPAREN }
	| '{'				{ LBRACE }
	| '}'				{ RBRACE }
	| ','				{ COMMA }
	| ';'				{ SEMI }
	| '='				{ ASSIGN }
	| '+'				{ PLUS }
	| '-'				{ MINUS }
	| '*'				{ TIMES }
	| '/'				{ DIVIDE }
	| '%'				{ MOD }
	| '<'				{ LT }
   	| "<="                		{ LEQ }
   	| '>'                  		{ GT }
   	| ">="                 		{ GEQ }
   	| "=="                 	 	{ EQ }
   	| "!="                 	 	{ NEQ }
   	| "&&"				{ AND }
	| "||"				{ OR }
	| '!'				{ NOT }
        | "true"			{ TRUE }
	| "false"			{ FALSE }
	| "if"				{ IF }
	| "else"			{ ELSE }
	| "while"			{ WHILE }
	| "return"			{ RETURN }
	| "int"				{ INT }			
	| "string"			{ STRING }
	| "char"			{ CHAR }
	| "bool"			{ BOOL }
	| "void"			{ VOID }
        | int    	as lxm { INT_LIT(int_of_string lxm ) }
	| string	{ STRING_LITERAL(s) }   
	| char          as lxm 	{ CHAR_LIT( String.get lxm 1) }
	| id as lxm { ID(lxm) }	
	| eof					{ EOF }
		 	 	 		
	and comment = parse
	| "##"					{ token lexbuf }	(* comment end *)
	| _					{ comment lexbuf }	(* eat everything else *)
	 

