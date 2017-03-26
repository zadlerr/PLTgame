{ open Printf }

rule token = parse
	[ ' ' '\t' '\r' '\n'] 	{ token lexbuf }	(* whitespace *)
	| "##"					{ comment lexbuf }	(* comment start *)	
        | '('					{ print_string "LPAREN " }		(* delimiters *)
	| ')'					{ print_string "RPAREN " }
	| '{'					{ print_string "LBRACE " }
	| '}'					{ print_string "RBRACE " }
	| ','					{ print_string "COMMA " }
	| ';'					{ print_string "SEMI " }
   
	 (*
	| '['					{ LBRACK }
	| ']'					{ RBRACK }
	| '.'					{ ACCESS }		(* operators *)
	| '*'					{ TIMES }
	| '/'					{ DIVIDE }
	| '%'					{ MOD }
	| '+'					{ PLUS }
	| '-'					{ MINUS }
	| '<'					{ LT }
   	| "<="                  { LEQ }
   	| '>'                   { GT }
   	| ">="                  { GEQ }
   	| "=="                  { EQ }
   	| "!="                  { NEQ }
   	| '&'					{ AND }
	| '|'					{ OR }
	| '!'					{ NOT }
	| '='					{ ASSIGN }
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
	| "return"				{ RETURN }
	| "string"				{ STRING }
	| "boolean"				{ BOOLEAN }
	| "void"				{ VOID }
        | "true"					as lxm { BOOL_LITERAL(bool_of_string lxm ) }
	| "false"				as lxm { BOOL_LITERAL(bool_of_string lxm ) }
	*)
	| "int"					{ print_string "INT " }			
	| [ 'a' - 'z'  'A' - 'Z' ][ 'a' - 'z'  'A' - 'Z'  '0' - '9'  '_' ]*   as lxm  { print_string "ID " }
        | ['0'- '9']+				as lxm { print_string "INT_LITERAL " }
	(*| eof					{ EOF } *)
		 	 	 		
	and comment = parse
	| "##"					{ token lexbuf }	(* comment end *)
	| _					{ comment lexbuf }	(* eat everything else *)


{
	let main () = 
	   let lexbuf = Lexing.from_channel stdin in
	   try
	      while true do
	         ignore (token lexbuf)
	      done
	   with _ -> print_string "invalid_token\n"
	let _ = Printexc.print main ()

}	
