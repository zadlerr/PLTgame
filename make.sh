ocamllex scanner.mll

ocamlyacc parser.mly

ocamlc -c ast.mli

ocamlc -c parser.mli

ocamlc -c scanner.ml

ocamlc -c parser.ml

ocamlc -o scanner.cmo parser.cmo
