open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module MU = MemUtil;;

let outChannel = open_out "memUtilUnit.cil.c";;

(* Set up a file for running tests *)
let inputFile = "memUtilUnit.c";;
let cilFile = makeCilFile inputFile;;

ignore (MakeOneCFG.feature.fd_doit cilFile);;
let _ = Ptranal.analyze_file cilFile;;
let _ = Ptranal.compute_results true;;
let _ = Ptranal.print_types ();;
ignore (AddAnnotations.feature.fd_doit cilFile);;

Stats.time "printCIL" 
  (dumpFile (!printerForMaincil) outChannel cilFile.fileName) cilFile;


(* Describe what we are interested in examining within CIL *)
type test_data = {
  (* Variables *)
  g : exp ref;
  g_a : exp ref;
  g_na : exp ref;
  test_point : exp ref;
  test_point_a : exp ref;
  (* Code Labels *)
  label_one : string;
  label_two : string;
  label_three : string;
  mutable id_one : int;
  mutable id_two : int;
  mutable id_three : int;
};;


let mem_util_data = {
  g = ref zero;
  g_a = ref zero;
  g_na = ref zero;
  test_point = ref zero;
  test_point_a = ref zero;
  label_one = "ONE";
  label_two = "TWO";
  label_three = "THREE";
  id_one = 0;
  id_two = 0;
  id_three = 0;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor

  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
        "g" ->
          ignore (printf "%s at %d\n" v.vname !currentLoc.line);
          mem_util_data.g := Lval (var v);
          SkipChildren
      | "g_a" ->
          ignore (printf "%s at %d\n" v.vname !currentLoc.line);
          mem_util_data.g_a := Lval (var v);
          SkipChildren
      | "g_na" ->
          mem_util_data.g_na := Lval (var v);
          SkipChildren
      | "test_point" ->
          mem_util_data.test_point := Lval (var v);
          SkipChildren
      | "test_point_a" ->
          mem_util_data.test_point_a := Lval (var v);
          SkipChildren
      | _ -> SkipChildren


  (* Snarf the states of interest *)
  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = mem_util_data.label_one -> 
             mem_util_data.id_one <- s.sid
         | Label (name, _, _) when name = mem_util_data.label_two -> 
             mem_util_data.id_two <- s.sid
         | Label (name, _, _) when name = mem_util_data.label_three -> 
             mem_util_data.id_three <- s.sid
         | Label (name, _, _) -> ignore (printf "Failed for label name: %s\n" name)
         | _ -> ignore (printf "Skipping statement %a\n" d_stmt s)
      )
      s.labels;
    SkipChildren
end


(* Generate  data for the tests *)
let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;

(*
val is_subexpression_of: Cil.exp -> Cil.exp -> bool;;
val may_alias_wrapper: Cil.exp -> Cil.exp -> bool;;
val get_claim: Cil.instr -> Cil.exp list;;
val get_released: Cil.instr -> Cil.exp list;;
*)

(* Tests!!! *)
let test_may_alias_wrapper = 
  
  MU.dbg_may_alias := true;
  
  ignore (printf "Expression %a resolves to:\n" d_exp !(mem_util_data.g));
  List.iter 
    (fun v -> ignore (printf "*** %s\n" v.vname)) 
    (Ptranal.resolve_exp !(mem_util_data.g));  
  
  ignore (printf "Expression %a resolves to:\n" d_exp !(mem_util_data.g_a));
  List.iter 
    (fun v -> ignore (printf "*** %s\n" v.vname)) 
    (Ptranal.resolve_exp !(mem_util_data.g_a));  
  
  let g_and_g_a = 
    MU.may_alias_wrapper 
      !(mem_util_data.g)
      !(mem_util_data.g_a)
  in
  
  let g_a_and_g_na = 
    not (MU.may_alias_wrapper !(mem_util_data.g_a) !(mem_util_data.g_na))
  in
  
  let test_point_and_test_point_a = 
    not (MU.may_alias_wrapper !(mem_util_data.test_point) !(mem_util_data.test_point_a))
  in
  
    TestCase(fun _ -> assert_bool "Incorrect MU.may_alias information" 
                      (g_and_g_a && g_a_and_g_na && test_point_and_test_point_a))
;;

(* Run all the tests *)
let suite_equivClone = 
  TestLabel ("MemUtil", 
             TestList [
               TestLabel ("memUtilUnit.c may_alias:", test_may_alias_wrapper);
             ]
  )
;;

let main = run_test_tt suite_equivClone

