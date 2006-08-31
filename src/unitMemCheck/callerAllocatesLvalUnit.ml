open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module MU = MemUtil;;
module CAL = CallerAllocatesLval;;

CAL.dbg_caller_allocates_lval_combine := false;;
CAL.dbg_caller_allocates_lval_i := false;;
CAL.dbg_caller_allocates_lval_s := false;;
IsEquivalent.dbg_equiv_get_equiv_set := false;;


(* Set up a file for running tests *)
let inputFile = "callerAllocatesLvalUnit.c";;
let cilFile = makeCilFile inputFile;;

ignore (MakeOneCFG.feature.fd_doit cilFile);;
ignore (AddAnnotations.feature.fd_doit cilFile);;

(*
let outChannel = open_out "memUtilUnit.cil.c";;
Stats.time "printCIL" 
  (dumpFile (!printerForMaincil) outChannel cilFile.fileName) cilFile;
 *)

(* Describe what we are interested in examining within CIL *)
type test_data = {
  ker_malloc : bool ref;
  ker_free : bool ref;
  make_graph_return : bool ref;
  make_graph_formal : bool ref;
  make_graph_formal_bad : bool ref;
  free_graph : bool ref;
};;


let mem_util_data = {
  ker_malloc = ref false;
  ker_free = ref false;
  make_graph_return = ref false;
  make_graph_formal = ref false;
  make_graph_formal_bad = ref false;
  free_graph = ref false;
};;



class testVisitor = object (self)
  inherit nopCilVisitor

  method vfunc (f:fundec) = 

    IsEquivalent.generate_equiv f;
    
    let check_alloc (vl: varinfo list) : bool =
      
      let allocated = 
        List.filter (fun v -> hasAttribute "sos_claim" v.vattr) vl
      in
        
        List.for_all (fun v -> CAL.lval_is_allocated v f) allocated
    in
           
    
      match f with 
          _ when (f.svar.vname = "ker_malloc") ->
            mem_util_data.ker_malloc :=  check_alloc f.sformals;
            DoChildren

        | _ when (f.svar.vname = "ker_free") ->
            mem_util_data.ker_free :=  check_alloc f.sformals;
            DoChildren

        | _ when (f.svar.vname = "make_graph_return") ->
            mem_util_data.make_graph_return :=  check_alloc f.sformals;
            DoChildren

        | _ when (f.svar.vname = "make_graph_formal") ->
            mem_util_data.make_graph_formal :=  check_alloc f.sformals;
            DoChildren

        | _ when (f.svar.vname = "make_graph_formal_bad") ->
            mem_util_data.make_graph_formal :=  not (check_alloc f.sformals);
            DoChildren

        | _ when (f.svar.vname = "free_graph") ->
            mem_util_data.free_graph :=  check_alloc f.sformals;
            DoChildren

        | _ -> DoChildren
        
end

let test_cal = 

  visitCilFileSameGlobals (new testVisitor) cilFile;

  TestCase(fun _ -> assert_bool "Incorrect MU.get_claim information" 
                      (
                        !(mem_util_data.ker_malloc) &&
                        !(mem_util_data.ker_free) &&
                        !(mem_util_data.make_graph_return) &&
                        !(mem_util_data.make_graph_formal) &&
                        !(mem_util_data.make_graph_formal_bad) &&
                        !(mem_util_data.free_graph) &&
                        true
                      )
  )
;;

(* Run all the tests *)
let suite_equivClone = 
  TestLabel ("CallerAllocatesLval", 
             TestList [
               TestLabel ("callerAllocatesLvalUnit.c test_cal:", test_cal);
             ]
  )
;;

let main = run_test_tt suite_equivClone

