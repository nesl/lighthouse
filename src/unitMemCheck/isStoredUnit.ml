open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module IE = IsEquivalent;;
module IS = IsStored;;


IS.dbg_is_store_i := false;;
IS.dbg_is_store_s := false;;
IS.dbg_is_store_g := false;;
IS.dbg_is_store_c := false;;

(* Set up a file for running tests *)
let inputFile = "isStoredUnit.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.make_one_cfg cilFile);;

let global_stores = 
  foldGlobals 
    cilFile
    (fun s g -> match g with
         GVarDecl (v, l) | GVar (v, _, l) -> (Lval (var v))::s
       | GFun (fd, l) -> s
       | _ -> s
    ) 
    []
;;


let stores (f:fundec) : exp list = 
  let local_stores = 
    List.map 
      (fun v -> (Lval (var v)))
      (List.filter (fun v -> hasAttribute "sos_store" v.vattr) (f.slocals @ f.sformals))
  in
    local_stores @ global_stores
;;


type test_data_type = {
  store_a : bool ref;
  store_b : bool ref;
  store_c : bool ref;
  store_d : bool ref;
  bad_store_a : bool ref;
  no_release : bool ref;
  main : bool ref;
};;


let test_data = {
  store_a = ref false;
  store_b = ref false;
  store_c = ref false;
  store_d = ref false;
  bad_store_a = ref false;
  no_release = ref false;
  main = ref false;
};;


(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor
  
  method vfunc (f:fundec) =

    IE.generate_equiv f cilFile;
    
    let bad_formal_store f = 
      List.filter  
        (fun v -> not (IS.is_stored_func (Lval (var v)) f (stores f)))
        (List.filter (fun v -> hasAttribute "sos_release" v.vattr) f.sformals)
    in
    
    let safely_stores (f:fundec) : bool =
      (List.length (bad_formal_store f)) = 0
    in

    
      match f with
          _ when (f.svar.vname = "store_a") ->
            test_data.store_a := safely_stores f;
            DoChildren
        | _ when (f.svar.vname = "store_b") ->
            test_data.store_b := safely_stores f;
            DoChildren
        | _ when (f.svar.vname = "store_c") ->
            test_data.store_c := safely_stores f;
            DoChildren
        | _ when (f.svar.vname = "store_d") ->
            test_data.store_d := safely_stores f;
            DoChildren
        | _ when (f.svar.vname = "bad_store_a") ->
            test_data.bad_store_a := safely_stores f;
            DoChildren
        | _ when (f.svar.vname = "no_release") ->
            test_data.no_release := safely_stores f;
            DoChildren
        | _ when (f.svar.vname = "main") ->
            test_data.main := safely_stores f;
            DoChildren
        | _ -> DoChildren

end


let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;

let test_store_a = 
  TestCase(fun _ -> assert_bool "store_a" !(test_data.store_a))
;; 

let test_store_b = 
  TestCase(fun _ -> assert_bool "store_b" !(test_data.store_b))
;; 

let test_store_c = 
  TestCase(fun _ -> assert_bool "store_c" !(test_data.store_c))
;; 

let test_store_d = 
  TestCase(fun _ -> assert_bool "store_d" !(test_data.store_d))
;; 

let test_bad_store_a = 
  TestCase(fun _ -> assert_bool "bad_store_a" (not !(test_data.bad_store_a)))
;; 

let test_no_release = 
  TestCase(fun _ -> assert_bool "no_release" !(test_data.no_release))
;; 

let test_main = 
  TestCase(fun _ -> assert_bool "main" !(test_data.main))
;; 

(* Run all the tests *)
let suite_is_stored = 
  TestLabel ("IsStored", 
             TestList [
               TestLabel ("IsStored: ", test_store_a);
               TestLabel ("IsStored: ", test_store_b);
               TestLabel ("IsStored: ", test_store_c);
               TestLabel ("IsStored: ", test_store_d);
               TestLabel ("IsStored: ", test_bad_store_a);
               TestLabel ("IsStored: ", test_no_release);
               TestLabel ("IsStored: ", test_main);
             ]
  )
;;

let main = run_test_tt suite_is_stored;;


