open Ast

let eval = function
    Lit(x) -> x

let _ = 
    let lexbuf = Lexing.from_channel stdin in
    let program_object = Parser.program Scanner.token lexbuf in
    let int2print = eval program_object in
    print_endline(string_of_int int2print)
