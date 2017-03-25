module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC" 
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t in


(*let function_decls = *)
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list []
(*	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) *)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    (*List.fold_left*) function_decl StringMap.empty functions;


  (*List.iter build_function_body functions;*)
  the_module
