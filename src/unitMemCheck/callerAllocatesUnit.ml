open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module MU = MemUtil;;
module CAL = CallerAllocates;;

CAL.dbg_caller_allocates_c := false;;
CAL.dbg_caller_allocates_i := false;;
CAL.dbg_caller_allocates_s := false;;
IsEquivalent.dbg_is_equiv_get_equiv_set := false;;
IsEquivalent.dbg_is_equiv_stmt_summary := false;;
IsEquivalent.dbg_is_equiv_get_aliases := false;;


(* Set up a file for running tests *)
let inputFile = "callerAllocatesUnit.c";;
let cilFile = makeCilFile inputFile;;

ignore (MakeOneCFG.make_one_cfg cilFile);;
ignore (AddAnnotations.feature.fd_doit cilFile);;

(*
let outChannel = open_out "memUtilUnit.cil.c";;
Stats.time "printCIL" 
  (dumpFile (!printerForMaincil) outChannel cilFile.fileName) cilFile;
 *)

(* Describe what we are interested in examining within CIL *)

type test_data = {
  lval_ker_malloc : bool ref;
  lval_ker_free : bool ref;
  lval_make_graph_return : bool ref;
  lval_make_graph_formal : bool ref;
  lval_make_graph_formal_bad_a : bool ref;
  lval_make_graph_formal_bad_b : bool ref;
  lval_free_graph : bool ref;
  return_ker_malloc : bool ref;
  return_ker_free : bool ref;
  return_make_graph_formal : bool ref;
  return_make_graph_return : bool ref;
  return_make_graph_return_bad_a : bool ref;
  return_make_graph_return_bad_b : bool ref;
  return_free_graph : bool ref;
  bad_allocate_a : bool ref;
  bad_allocate_b : bool ref;
};;


let mem_util_data = {
  lval_ker_malloc = ref false;
  lval_ker_free = ref false;
  lval_make_graph_return = ref false;
  lval_make_graph_formal = ref false;
  lval_make_graph_formal_bad_a = ref false;
  lval_make_graph_formal_bad_b = ref false;
  lval_free_graph = ref false;
  return_ker_malloc = ref false;
  return_ker_free = ref false;
  return_make_graph_formal = ref false;
  return_make_graph_return = ref false;
  return_make_graph_return_bad_a = ref false;
  return_make_graph_return_bad_b = ref false;
  return_free_graph = ref false;
  bad_allocate_a = ref false;
  bad_allocate_b = ref false;
};;



class testVisitorLval = object (self)
  inherit nopCilVisitor

  method vfunc (f:fundec) = 

    IsEquivalent.generate_equiv f cilFile;
    
    let check_alloc_lval (vl: varinfo list) : bool =
      
      let allocated = 
        List.filter (fun v -> hasAttribute "sos_claim" v.vattr) vl
      in
        
        List.for_all (fun v -> CAL.var_is_allocated v f) allocated
    in
           
    
      match f with 
          _ when (f.svar.vname = "ker_malloc") ->
            mem_util_data.lval_ker_malloc :=  check_alloc_lval f.sformals;
            DoChildren

        | _ when (f.svar.vname = "ker_free") ->
            mem_util_data.lval_ker_free :=  check_alloc_lval f.sformals;
            DoChildren

        | _ when (f.svar.vname = "make_graph_return") ->
            mem_util_data.lval_make_graph_return :=  check_alloc_lval f.sformals;
            DoChildren

        | _ when (f.svar.vname = "make_graph_formal") ->
            mem_util_data.lval_make_graph_formal :=  check_alloc_lval f.sformals;
            DoChildren

        | _ when (f.svar.vname = "make_graph_formal_bad_a") ->
            mem_util_data.lval_make_graph_formal_bad_a :=  not (check_alloc_lval f.sformals);
            DoChildren

        | _ when (f.svar.vname = "make_graph_formal_bad_b") ->
            mem_util_data.lval_make_graph_formal_bad_b :=  not (check_alloc_lval f.sformals);
            DoChildren

        | _ when (f.svar.vname = "free_graph") ->
            mem_util_data.lval_free_graph :=  check_alloc_lval f.sformals;
            DoChildren

        | _ -> DoChildren
        
end



let test_lval = 

  visitCilFileSameGlobals (new testVisitorLval) cilFile;

  TestCase(fun _ -> assert_bool "Incorrect MU.get_claim information" 
                      (
                        !(mem_util_data.lval_ker_malloc) &&
                        !(mem_util_data.lval_ker_free) &&
                        !(mem_util_data.lval_make_graph_return) &&
                        !(mem_util_data.lval_make_graph_formal) &&
                        !(mem_util_data.lval_make_graph_formal_bad_a) &&
                        !(mem_util_data.lval_make_graph_formal_bad_b) &&
                        !(mem_util_data.lval_free_graph) &&
                        true
                      )
  )
;;



class testVisitorReturn = object (self)
  inherit nopCilVisitor

  method vfunc (f:fundec) = 

    IsEquivalent.generate_equiv f cilFile;
    
      match f with 
          _ when (f.svar.vname = "ker_malloc") ->
            mem_util_data.return_ker_malloc :=  not (CAL.return_is_allocated f);
            DoChildren

        | _ when (f.svar.vname = "ker_free") ->
            mem_util_data.return_ker_free :=  not (CAL.return_is_allocated f);
            DoChildren

        | _ when (f.svar.vname = "make_graph_formal") ->
            mem_util_data.return_make_graph_formal :=  not (CAL.return_is_allocated f);
            DoChildren
        | _ when (f.svar.vname = "make_graph_return") ->
            mem_util_data.return_make_graph_return :=  CAL.return_is_allocated f;
            DoChildren

        | _ when (f.svar.vname = "make_graph_return_bad_a") ->
            mem_util_data.return_make_graph_return_bad_a :=  not (CAL.return_is_allocated f);
            DoChildren

        | _ when (f.svar.vname = "make_graph_return_bad_b") ->
            mem_util_data.return_make_graph_return_bad_b :=  not (CAL.return_is_allocated f);
            DoChildren

        | _ when (f.svar.vname = "free_graph") ->
            mem_util_data.return_free_graph :=  not (CAL.return_is_allocated f);
            DoChildren

        | _ -> DoChildren
        
end

let test_return = 

  visitCilFileSameGlobals (new testVisitorReturn) cilFile;

  TestCase(fun _ -> assert_bool "Incorrect MU.get_claim information" 
                      (
                        !(mem_util_data.return_ker_malloc) &&
                        !(mem_util_data.return_ker_free) &&
                        !(mem_util_data.return_make_graph_formal) &&
                        !(mem_util_data.return_make_graph_return) &&
                        !(mem_util_data.return_make_graph_return_bad_a) &&
                        !(mem_util_data.return_make_graph_return_bad_b) &&
                        !(mem_util_data.return_free_graph) &&
                        true
                      )
  )
;;



(*
 * Both of these functions test to see if the claims list is being properly 
 * updated within the CallerAllocates module.  At the time of writing, both of
 * these tests fail :-(  
 *)
class testVisitorBadAllocate = object (self)
  inherit nopCilVisitor

  method vfunc (f:fundec) =

    IsEquivalent.generate_equiv f cilFile;
    
    let check_alloc_lval (vl: varinfo list) : bool =
      
      let allocated = 
        List.filter (fun v -> hasAttribute "sos_claim" v.vattr) vl
      in
        
        List.for_all (fun v -> CAL.var_is_allocated v f) allocated
    in
           
    
      match f with 
          _ when (f.svar.vname = "bad_allocate_a") ->
            mem_util_data.bad_allocate_a :=  not (check_alloc_lval f.sformals);
            DoChildren

        | _ when (f.svar.vname = "bad_allocate_b") ->
            mem_util_data.bad_allocate_b :=  not (check_alloc_lval f.sformals);
            DoChildren

        | _ -> DoChildren
end


let test_bad_allocate_a = 
  visitCilFileSameGlobals (new testVisitorBadAllocate) cilFile;
  TestCase(fun _ -> 
             assert_bool "Incorrect upkeep of allocated data"
               !(mem_util_data.bad_allocate_a) 
  )
;;


let test_bad_allocate_b = 
  visitCilFileSameGlobals (new testVisitorBadAllocate) cilFile;
  TestCase(fun _ -> 
             assert_bool "Incorrect upkeep of allocated data"
               !(mem_util_data.bad_allocate_b) 
  )
;;


(* Run all the tests *)
let suite_equivClone = 
  TestLabel ("CallerAllocatesLval", 
             TestList [
               TestLabel ("callerAllocatesUnit.c test_lval:", test_lval);
               TestLabel ("callerAllocatesUnit.c test_return:", test_return);
               TestLabel ("Upkeep", test_bad_allocate_a);
               TestLabel ("Upkeep", test_bad_allocate_b);
             ]
  )
;;

let main = run_test_tt suite_equivClone

