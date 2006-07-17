open Cil
open Pretty

class makeOneCFG = object
  inherit nopCilVisitor 

  method vstmt s =
    
    match s.skind with
      Instr(il) -> begin
        let list_of_stmts = List.map (fun one_inst -> 
          mkStmtOneInstr one_inst) il in
        let block = mkBlock list_of_stmts in
        ChangeDoChildrenPost(s, (fun _ -> 
          s.skind <- Block(block) ;
          s))
      end
    | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end 


let one_inst_in_basic_blocks f =
  let thisVisitor = new makeOneCFG in
  visitCilFileSameGlobals thisVisitor f  


let feature : featureDescr = {
  fd_name = "makeOneCFG";
  fd_enabled = ref true;
  fd_description = "make the program look more like a CFG with one instruction per statement";
  fd_extraopt = [];
  fd_doit = 
    (fun f -> 
       ignore (one_inst_in_basic_blocks f) ; 
       ignore (Partial.globally_unique_vids f) ; 
       iterGlobals f (fun glob -> match glob with
                          GFun(fd,_) -> prepareCFG fd ;
                                        ignore (computeCFGInfo fd true)
                        | _ -> ()) 
  );
  fd_post_check = true;
  } 

