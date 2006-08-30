open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module MA = MayAliasWrapper;;


(* Set up a file for running tests *)
let inputFile = "mayAliasWrapperUnit.c";;
let cilFile = makeCilFile inputFile;;

ignore (MakeOneCFG.feature.fd_doit cilFile);;
let _ = Ptranal.analyze_file cilFile;;
let _ = Ptranal.compute_results false;;

(* Describe what we are interested in examining within CIL *)
type test_data = {
  (* Variables *)
  g : exp ref;
  ga : exp ref;
  gna : exp ref;
  p : exp ref;
  pa : exp ref;
  (* Code Labels *)
  label_one : string;
  label_two : string;
  label_three : string;
  mutable id_one : int;
  mutable id_two : int;
  mutable id_three : int;
  ker_malloc : bool ref;
  ker_free : bool ref;
  make_graph_return : bool ref;
  make_graph_formal : bool ref;
  free_graph : bool ref;
};;


let mem_util_data = {
  g = ref zero;
  ga = ref zero;
  gna = ref zero;
  p = ref zero;
  pa = ref zero;
  label_one = "ONE";
  label_two = "TWO";
  label_three = "THREE";
  id_one = 0;
  id_two = 0;
  id_three = 0;
  ker_malloc = ref true;
  ker_free = ref true;
  make_graph_return = ref true;
  make_graph_formal = ref true;
  free_graph = ref true;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor

  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
      | "g" ->
          mem_util_data.g := Lval (var v);
          SkipChildren
      | "ga" ->
          mem_util_data.ga := Lval (var v);
          SkipChildren
      | "gna" ->
          mem_util_data.gna := Lval (var v);
          SkipChildren
      | "p" ->
          mem_util_data.p := Lval (var v);
          SkipChildren
      | "pa" ->
          mem_util_data.pa := Lval (var v);
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
    DoChildren


end


(* Generate  data for the tests *)
let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;

(*
val get_claim: Cil.instr -> Cil.exp list;;
val get_released: Cil.instr -> Cil.exp list;;
*)

(* Tests!!! *)
let test_may_alias_wrapper = 
  
  let g_and_ga = 
    MA.may_alias_wrapper 
      !(mem_util_data.g)
      !(mem_util_data.ga)
  in
  
  let ga_and_g =
    MA.may_alias_wrapper 
      !(mem_util_data.ga)
      !(mem_util_data.g)
  in
  
  let g_and_gna =
    not
      (MA.may_alias_wrapper 
         !(mem_util_data.g)
         !(mem_util_data.gna)
      )
  in
  
  let gna_and_ga =
    not
      (MA.may_alias_wrapper 
         !(mem_util_data.gna)
         !(mem_util_data.ga)
      )
  in
  
    TestCase(fun _ -> assert_bool "Incorrect MA.may_alias information" 
                        (g_and_ga && ga_and_g && g_and_gna && gna_and_ga))
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

