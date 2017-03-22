%{ open Ast %}

%token <int> INT_LITERAL

%token EOF

%start program
%type <Ast.program> program

%%

program: INT_LITERAL { Lit($1) }
