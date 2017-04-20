module L = Llvm
(*open Ast
*)
module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC" 
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and ptr_t  = L.pointer_type
  and void_t = L.void_type context 
  and str_t  = L.pointer_type (L.i8_type context) in

  let ltype_of_typ = function
      Ast.Int -> i32_t
    | Ast.Bool -> i1_t
    | Ast.Void -> void_t
    | Ast.Char -> i8_t
    | Ast.String -> str_t in

  (* Declare each global variable; remember its value in a map *)
  let global_vars = 
	let global_var m (t, n) =
	  let init = L.const_int (ltype_of_typ t) 0
          in StringMap.add n (L.define_global n init the_module) m in
	    List.fold_left global_var StringMap.empty globals
	      in

  (*Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] 
    in
    let printf_func = L.declare_function "printf" printf_t the_module 
      in

  (* Ignoring printbig for now too *)
  

  (* Define each function (arguments and return type ) so we can call it  *)
  let function_decls = 
    let function_decl m fdecl =
      let name = fdecl.Ast.fname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.Ast.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.Ast.typ) formal_types 
        in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m     	
          in
          List.fold_left function_decl StringMap.empty functions 
	    in


(* need to build function bodies *)
(* by building expressions *)
(* by building statements *)
 
(* Fill in the body of the given function *)
  let build_function_body fdecl =   (* function to build body of function *)
    let (the_function, _) = StringMap.find fdecl.Ast.fname function_decls 
      in (* find function declaration from map *)
      let builder = L.builder_at_end context (L.entry_block the_function) 
        in (* builds at end of basic block. A basic block is simply a container of instructions that execute sequentially*)
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
          in 
	let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in(* llvm int to string. let's see where it's used *)
	let char_format_str = L.build_global_stringptr "%c" "fmt" builder
        in 

 
    let local_vars = 
      let add_formal m (t, n) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m 
          in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	  in
	  StringMap.add n local_var m
	    in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.Ast.formals (Array.to_list (L.params the_function))
      in
      (* line add locals to local_vars. Formals already in local_vars *)
        List.fold_left add_local formals fdecl.Ast.locals
        in

     (* Return the value for a variable or formal argument *)
       let lookup n = try StringMap.find n local_vars
                      with Not_found -> StringMap.find n global_vars
       in
     let type_of_val = function 
             "i32*" -> int_format_str (*int*)
           | "i8**" -> str_format_str (*string*)
           | "i8*" -> char_format_str (*char*)
           | "i1*" -> int_format_str (*bool*)
           | _ -> str_format_str
         in 
     
         let check_print_input = function
             Ast.Int_Literal e -> int_format_str
           | Ast.String_Lit e -> str_format_str 
           | Ast.Char_Literal c -> char_format_str 
           | Ast.Binop (e1, op, e2) -> int_format_str 
           | Ast.BoolLit b -> int_format_str 
           | Ast.Id s -> type_of_val(L.string_of_lltype(L.type_of (lookup s)))
           | Ast.Assign (s, e) -> int_format_str
           | Ast.Noexpr -> int_format_str
           | Ast.Unop (op, e) -> int_format_str
           | Ast.Call (s, actuals) -> int_format_str
         in 
 
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	Ast.Int_Literal i -> L.const_int i32_t i
      | Ast.String_Lit s -> L.build_global_stringptr s "str" builder 
      | Ast.Char_Literal c -> L.const_int i8_t (int_of_char c)
      | Ast.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | Ast.Noexpr -> L.const_int i32_t 0
      (* for formals lookup need Id. Need Call for 'printf' *)
      | Ast.Id s -> L.build_load (lookup s) s builder
      | Ast.Assign (s, e) -> let e' = expr builder e in 
		ignore (L.build_store e' (lookup s) builder); e'
      | Ast.Binop(e1, op, e2) ->
		let e1' = expr builder e1 and e2' = expr builder e2 in
		(match op with 
		    Ast.Add  -> L.build_add
		  | Ast.Sub  -> L.build_sub
		  | Ast.Mult -> L.build_mul
		  | Ast.Div  -> L.build_sdiv
		  | Ast.Mod  -> L.build_srem
	  	  | Ast.And     -> L.build_and
	  	  | Ast.Or      -> L.build_or
		  | Ast.Equal-> L.build_icmp L.Icmp.Eq
		  | Ast.Neq  -> L.build_icmp L.Icmp.Ne
		  | Ast.Lessthan  -> L.build_icmp L.Icmp.Slt
		  | Ast.Greaterthan -> L.build_icmp L.Icmp.Sgt
		  | Ast.Leq -> L.build_icmp L.Icmp.Sle
		  | Ast.Geq -> L.build_icmp L.Icmp.Sge

		) e1' e2' "tmp" builder
      | Ast.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    Ast.Neg     -> L.build_neg
          | Ast.Not     -> L.build_not) e' "tmp" builder
      | Ast.Call ("print", [e]) ->
    	  L.build_call printf_func [| check_print_input e ; (expr builder e) |]
	        "printf" builder
(*
      | Ast.Call ("prints", [e]) ->
	    let get_string = function Ast.String_Lit s -> s 
	  	| _ -> "" 
		in 
		let s_ptr = L.build_global_stringptr ((get_string e) ^ "\n") ".str" builder in 
		L.build_call printf_func [| s_ptr |] "printf" builder
*)           
	| Ast.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.Ast.typ with Ast.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
        
	in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
      	  Some _ -> ()
        | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	Ast.Block sl -> List.fold_left stmt builder sl
      | Ast.Expr e -> ignore (expr builder e); builder
      | Ast.Return e -> ignore (match fdecl.Ast.typ with
    	    Ast.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder); builder
      | Ast.If (predicate, then_stmt, else_stmt) ->
	let bool_val = expr builder predicate in
	  let merge_bb = L.append_block context "merge" the_function in
	   let then_bb = L.append_block context "then" the_function in
	     add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	       (L.build_br merge_bb);

	   let else_bb = L.append_block context "else" the_function in
	     add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	       (L.build_br merge_bb);
	
	   ignore (L.build_cond_br bool_val then_bb else_bb builder);
	   L.builder_at_end context merge_bb
     
      | Ast.While (predicate, body) ->	
	  let pred_bb = L.append_block context "while" the_function in
	    ignore (L.build_br pred_bb builder);
	    let body_bb = L.append_block context "while_body" the_function
	      in
	      add_terminal (stmt (L.builder_at_end context body_bb) body)
	        (L.build_br pred_bb);

	        let pred_builder = L.builder_at_end context pred_bb in
		  let bool_val = expr pred_builder predicate in
	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb
 
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (Ast.Block fdecl.Ast.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.Ast.typ with
     Ast.Void -> L.build_ret_void
     | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
   
    in
    List.iter build_function_body functions;
  the_module
