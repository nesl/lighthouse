open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module MU = MemUtil;;


(* Set up a file for running tests *)
let inputFile = "memUtilUnit.c";;
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
let test_is_parent_of = 
  
  let ga_arrow_edge = 
    Formatcil.cExp "%e:ga -> edges"
      ["ga", Fe !(mem_util_data.ga)]
  in

  let ga_arrow_point = 
    Formatcil.cExp "%e:ga -> points"
      ["ga", Fe !(mem_util_data.ga)]
  in

  let ga_arrow_edge_index = 
    Formatcil.cExp "%e:ga -> edges + 4"
      ["ga", Fe !(mem_util_data.ga)]
  in


  let ga_sub_of_ga_arrow_edge =
    MU.is_parent_of !(mem_util_data.ga) ga_arrow_edge
  in
        
  let not_ga_arrow_edge_sub_of_ga_arrow_point =
    not (MU.is_parent_of ga_arrow_edge ga_arrow_point)
  in
        
  let ga_sub_of_ga_arrow_edge_index =
    MU.is_parent_of !(mem_util_data.ga) ga_arrow_edge_index
  in
        
  let ga_arrow_edge_sub_of_ga_arrow_edge_index =
    MU.is_parent_of ga_arrow_edge ga_arrow_edge_index
  in
        
  let not_ga_arrow_edge_sub_of_ga =
    not (MU.is_parent_of ga_arrow_edge !(mem_util_data.ga))
  in
        
  
    TestCase(fun _ -> assert_bool "Incorrect MU.is_parent_of information" 
                        ((ga_sub_of_ga_arrow_edge) &&
                         (not_ga_arrow_edge_sub_of_ga_arrow_point) &&
                         (ga_sub_of_ga_arrow_edge_index) &&
                         (ga_arrow_edge_sub_of_ga_arrow_edge_index) &&
                         (not_ga_arrow_edge_sub_of_ga)
                        )
    )
;;



class getVisitor = object (self)
  inherit nopCilVisitor

   

  method vinst (i:instr) = 

    let test_claim_release (i: instr) (c: int) (r: int) : bool = 
      (List.length (MU.get_claim i) = c) && 
      (List.length (MU.get_released i) = r)
    in
    
      match i with 
          Call (lop, Lval (Var v, NoOffset), el, _) when (v.vname = "ker_malloc") ->
            mem_util_data.ker_malloc := 
            !(mem_util_data.ker_malloc) && test_claim_release i 1 0;
            DoChildren

        | Call (lop, Lval (Var v, NoOffset), el, _) when (v.vname = "ker_free") ->
            mem_util_data.ker_free := 
            !(mem_util_data.ker_free) && test_claim_release i 0 1; 
            DoChildren

        | Call (lop, Lval (Var v, NoOffset), el, _) when (v.vname = "make_graph_return") ->
            mem_util_data.make_graph_return := 
            !(mem_util_data.make_graph_return) && test_claim_release i 1 0; 
            DoChildren

        | Call (lop, Lval (Var v, NoOffset), el, _) when (v.vname = "make_graph_formal") ->
            mem_util_data.make_graph_formal := 
            !(mem_util_data.make_graph_formal) && test_claim_release i 1 0; 
            DoChildren

        | Call (lop, Lval (Var v, NoOffset), el, _) when (v.vname = "free_graph") ->
            mem_util_data.free_graph := 
            !(mem_util_data.free_graph) && test_claim_release i 0 1; 
            DoChildren

        | _ -> DoChildren
  
        
end

let test_get = 

  visitCilFileSameGlobals (new getVisitor) cilFile;

  TestCase(fun _ -> assert_bool "Incorrect MU.get_claim information" 
                      (
                        !(mem_util_data.ker_malloc) &&
                        !(mem_util_data.ker_free) &&
                        !(mem_util_data.make_graph_return) &&
                        !(mem_util_data.make_graph_formal) &&
                        !(mem_util_data.free_graph)
                      )
  )
;;

(* Run all the tests *)
let suite_equivClone = 
  TestLabel ("MemUtil", 
             TestList [
               TestLabel ("memUtilUnit.c is_parent_of:", test_is_parent_of);
               TestLabel ("memUtilUnit.c get_claim / get_released:", test_get);
             ]
  )
;;

let main = run_test_tt suite_equivClone

