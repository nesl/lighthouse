open OUnit
open CilDriver
open Pretty
open Cil

module IE = IsEquivalent
module IH = Inthash

(* Set up a file for running tests *)
let inputFile = "unitIsEquivalent01.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.feature.fd_doit cilFile);;

(* Describe what we are interested in examining within CIL *)
type test_data = {
  x : exp ref;
  y : exp ref;
  y_ptr : exp ref;
  y_ptr_ptr : exp ref;
  alias_ptr : exp ref;
  alias_ptr_ptr : exp ref;
  label_check : string;
  mutable id_check : int;
};;


let equiv_test_data = {
  x = ref zero;
  y = ref zero;
  y_ptr = ref zero;
  y_ptr_ptr = ref zero;
  alias_ptr = ref zero;
  alias_ptr_ptr = ref zero;
  label_check = "CHECK";
  id_check = 0;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor

  (* Prepare equiv clone information *)
  method vfunc (f:fundec) =
    IE.dbg_equiv_i := false;
    IE.dbg_equiv_combine := false;
    IE.dbg_equiv_stmt_summary := true;
    IE.dbg_equiv_get_aliases := false;
    IE.dbg_equiv_get_equiv_set := true;
    IE.dbg_equiv_df := false;

    IE.generate_equiv f;
    DoChildren


  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
        "x" ->
          equiv_test_data.x := Lval (var v);
          SkipChildren
      | "y" ->
          equiv_test_data.y := Lval (var v);
          SkipChildren
      | "y_ptr" ->
          equiv_test_data.y_ptr := Lval (var v);
          SkipChildren
      | "y_ptr_ptr" ->
          equiv_test_data.y_ptr_ptr := Lval (var v);
          SkipChildren
      | "alias_ptr" ->
          equiv_test_data.alias_ptr := Lval (var v);
          SkipChildren
      | "alias_ptr_ptr" ->
          equiv_test_data.alias_ptr_ptr := Lval (var v);
          SkipChildren
      | _ -> SkipChildren


  (* Snarf the states of interest *)
  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = equiv_test_data.label_check -> 
             equiv_test_data.id_check <- s.sid
         | Label (name, _, _) -> ignore (printf "Failed for label name: %s\n" name)
         | _ -> ignore (printf "Skipping statement %a\n" d_stmt s)
      )
      s.labels;
    SkipChildren
end

(* Generate  data for the tests *)
let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;

(* Helper function to check that an expression is considerd Dead by the clone
 * analysis *)
let is_dead (e:exp) (id:int) : bool =
  match IE.get_equiv_set e id with
      [e] -> true
    | [] -> true
    | _ -> false
;;


let star (e:exp) : exp = (Lval (mkMem e NoOffset));;
  

(* Tests!!! *)
let test_check = 
  let x_y = 
    IE.is_equiv 
      !(equiv_test_data.x)
      !(equiv_test_data.y)
      equiv_test_data.id_check
  in
  let ap_x = 
    IE.is_equiv 
      (star !(equiv_test_data.alias_ptr))
      !(equiv_test_data.x)
      equiv_test_data.id_check
  in
    TestCase(fun _ -> assert_bool "Incorrect clone information" (x_y && ap_x))
   (*
      TestCase(fun _ -> assert_bool "Incorrect clone information" (x_y && true))
    *)
;;




(* Run all the tests *)
let suite_check = 
  TestLabel ("Add Annotations", 
             TestList [
               TestLabel ("equivClone01.c: Check", test_check);
             ]
  )
;;

let main = run_test_tt suite_check

