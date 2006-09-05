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

(* Describe what we are interested in examining within CIL *)
type test_data_type = {
  pa : exp ref;
  pb : exp ref;
  pc : exp ref;
  fd : exp ref;
  sa : fundec ref;
  sb : fundec ref;
  sc : fundec ref;
  fa : fundec ref;
  main : fundec ref;
  stmt_one : stmt ref;
};;


let test_data = {
  pa = ref zero;
  pb = ref zero;
  pc = ref zero;
  fd = ref zero;
  sa = ref dummyFunDec;
  sb = ref dummyFunDec;
  sc = ref dummyFunDec;
  fa = ref dummyFunDec;
  main = ref dummyFunDec;
  stmt_one = ref dummyStmt;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor
  
  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
      | "pa" ->
          test_data.pa := Lval (var v);
          SkipChildren
      | "pb" ->
          test_data.pb := Lval (var v);
          SkipChildren
      | "pc" ->
          test_data.pc := Lval (var v);
          SkipChildren
      | "fd" ->
          test_data.fd := Lval (var v);
          SkipChildren
      | _ -> SkipChildren

  
  method vfunc (f:fundec) = match f with
      _ when (f.svar.vname = "store_a") ->
          test_data.sa := f;
          DoChildren
    | _ when (f.svar.vname = "store_b") ->
          test_data.sb := f;
          DoChildren
    | _ when (f.svar.vname = "store_c") ->
          test_data.sc := f;
          DoChildren
    | _ when (f.svar.vname = "fail_a") ->
          test_data.fa := f;
          DoChildren
    | _ when (f.svar.vname = "main") ->
          test_data.main := f;
          DoChildren
    | _ -> DoChildren


  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = "ONE" -> 
             test_data.stmt_one := s
         | Label (name, _, _) -> ignore (printf "Failed for label name: %s\n" name)
         | _ -> ignore (printf "Skipping statement %a\n" d_stmt s)
      )
      s.labels;
    SkipChildren

end


(* Generate  data for the tests *)
let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;


let get_released_exps (f:fundec) : exp list =

  let attr_name = "sos_release" in

  let (_, formals_op, _, _) = splitFunctionType f.svar.vtype in

  let released_formal_exps = match formals_op with
      Some attr_list ->
        List.fold_left2 
          (fun has_attribute (_, formal_type, formal_attrs) formal -> 
             if (hasAttribute attr_name formal_attrs) then
               (Lval (var formal))::has_attribute
             else has_attribute
          )
          []
          attr_list
          f.sformals
    | None -> []
  in

    released_formal_exps
;;


let stores (f:fundec) : exp list = 

  let local_stores = 
    List.map 
      (fun v -> (Lval (var v)))
      (List.filter (fun v -> hasAttribute "sos_store" v.vattr) f.slocals)
  in

    local_stores @ global_stores
;;


(* Tests!!! *)
let test_store_a = 
  let test = 
    IE.generate_equiv !(test_data.sa) cilFile;
    List.for_all
      (fun e -> IS.is_stored_func e !(test_data.sa) (stores !(test_data.sa)))
      (get_released_exps !(test_data.sa))
  in
    TestCase(fun _ -> assert_bool "store_a" test) 
;;


let test_store_b = 
  let test = 
    IE.generate_equiv !(test_data.sb) cilFile;
    List.for_all
      (fun e -> IS.is_stored_func e !(test_data.sb) (stores !(test_data.sb)))
      (get_released_exps !(test_data.sb))
  in
    TestCase(fun _ -> assert_bool "store_b" test) 
;;


let test_store_c = 
  let test = 
    IE.generate_equiv !(test_data.sc) cilFile;
 
    List.for_all
      (fun e -> IS.is_stored_func e !(test_data.sc) (stores !(test_data.sc)))
      (get_released_exps !(test_data.sc))
  in
    
    TestCase(fun _ -> assert_bool "store_c" test) 
;;


let test_fail_a = 
  let test = 
    IE.generate_equiv !(test_data.fa) cilFile;
    not (
      List.for_all
        (fun e -> IS.is_stored_func e !(test_data.fa) (stores !(test_data.fa)))
        (get_released_exps !(test_data.fa))
    )
  in
    TestCase(fun _ -> assert_bool "fail_a" test) 
;;


let test_pa = 
  let test = 
    IE.generate_equiv !(test_data.main) cilFile;
    IS.is_stored_instr 
      !(test_data.pa)
      !(test_data.stmt_one) 
      dummyInstr      
      !(test_data.main) 
      (stores !(test_data.main))
  in
    TestCase(fun _ -> assert_bool "pa" test) 
;;

let test_pb = 
  let test = 
    IE.generate_equiv !(test_data.main) cilFile;
    IS.is_stored_instr 
      !(test_data.pb)
      !(test_data.stmt_one) 
      dummyInstr      
      !(test_data.main) 
      (stores !(test_data.main))
  in
    TestCase(fun _ -> assert_bool "pb" test) 
;;


let test_pc = 
  let test = 
    IE.generate_equiv !(test_data.main) cilFile;
    IS.is_stored_instr 
      !(test_data.pc)
      !(test_data.stmt_one) 
      dummyInstr      
      !(test_data.main) 
      (stores !(test_data.main))
  in
    TestCase(fun _ -> assert_bool "pc" test) 
;;


let test_fd = 
  let test =
    not ( 
      IE.generate_equiv !(test_data.main) cilFile;
      IS.is_stored_instr 
        !(test_data.fd)
        !(test_data.stmt_one) 
        dummyInstr      
        !(test_data.main) 
        (stores !(test_data.main))
    )
  in
    TestCase(fun _ -> assert_bool "fd" test) 
;;




(* Run all the tests *)
let suite_is_stored = 
  TestLabel ("IsStored", 
             TestList [
               TestLabel ("IsStored: ", test_store_a);
               TestLabel ("IsStored: ", test_store_b);
               TestLabel ("IsStored: ", test_store_c);
               TestLabel ("IsStored: ", test_fail_a);
               TestLabel ("IsStored: ", test_pa);
               TestLabel ("IsStored: ", test_pb);
               TestLabel ("IsStored: ", test_pc);
               TestLabel ("IsStored: ", test_fd);
             ]
  )
;;



let main = run_test_tt suite_is_stored;;

