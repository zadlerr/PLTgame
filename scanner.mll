{ open Parser }


let ascii = ([' '-'!' '#'-'[' ']'-'~'])

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
	| "return"			{ RETURN }
	| "int"				{ INT }			
	| "string"			{ STRING }
	| "char"			{ CHAR }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9'  '_']* as lxm  { ID(lxm) }
        | ['0'-'9']+			as lxm { INT_LIT(int_of_string lxm ) }
	| '"' ([^'"']* as lit) '"'	{ STRING_LIT(lit) }   
	| '''(ascii | ['0'-'9']) ''' as lxm 			{ CHAR_LIT( String.get lxm 1) }
	| eof					{ EOF }
	
	 (*
	| '['					{ LBRACK }
	| ']'					{ RBRACK }
	| '.'					{ ACCESS }		(* operators *)
   	| '&'					{ AND }
	| '|'					{ OR }
	| '!'					{ NOT }
	| "fun"					{ FUN }			(* keywords *)
	| "events"				{ EVENTS }
	| "room"				{ ROOM }
	| "item"				{ ITEM }
	| "start"				{ START }
	| "npc"					{ NPC }
	| "end"					{ END }
	| "if"					{ IF }
	| "else"				{ ELSE }
	| "while"				{ WHILE }
	| "boolean"				{ BOOLEAN }
	| "void"				{ VOID }
        | "true"					as lxm { BOOL_LITERAL(bool_of_string lxm ) }
	| "false"				as lxm { BOOL_LITERAL(bool_of_string lxm ) }
	*)
		 	 	 		
	and comment = parse
	| "##"					{ token lexbuf }	(* comment end *)
	| _					{ comment lexbuf }	(* eat everything else *)

