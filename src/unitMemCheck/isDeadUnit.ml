open OUnit;;
open CilDriver;;
open Pretty;;
open Cil;;

module ID = IsDead;;


(* Set up a file for running tests *)
let inputFile = "isDeadUnit.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.make_one_cfg cilFile);;
ignore (Ptranal.analyze_file cilFile);;
ignore (Ptranal.compute_results false);;

(* Describe what we are interested in examining within CIL *)
type test_data = {
  safe_a : exp ref;
  safe_b : exp ref;
  unsafe_a : exp ref;
  unsafe_b : exp ref;
  unsafe_c : exp ref;
  unsafe_d : exp ref;
  unsafe_e : exp ref;
  unsafe_f : exp ref;
  unsafe_g : exp ref;
  label_one : string;
  stmt_one : stmt ref;
};;


let is_dead_test_data = {
  safe_a = ref zero;
  safe_b = ref zero;
  unsafe_a = ref zero;
  unsafe_b = ref zero;
  unsafe_c = ref zero;
  unsafe_d = ref zero;
  unsafe_e = ref zero;
  unsafe_f = ref zero;
  unsafe_g = ref zero;
  label_one = "ONE";
  stmt_one = ref dummyStmt;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor

  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
        "safe_a" ->
          is_dead_test_data.safe_a := Lval (var v);
          SkipChildren
      | "safe_b" ->
          is_dead_test_data.safe_b := Lval (var v);
          SkipChildren
      | "unsafe_a" ->
          is_dead_test_data.unsafe_a := Lval (var v);
          SkipChildren
      | "unsafe_b" ->
          is_dead_test_data.unsafe_b := Lval (var v);
          SkipChildren
      | "unsafe_c" ->
          is_dead_test_data.unsafe_c := Lval (var v);
          SkipChildren
      | "unsafe_d" ->
          is_dead_test_data.unsafe_d := Lval (var v);
          SkipChildren
      | "unsafe_e" ->
          is_dead_test_data.unsafe_e := Lval (var v);
          SkipChildren
      | "unsafe_f" ->
          is_dead_test_data.unsafe_f := Lval (var v);
          SkipChildren
      | "unsafe_g" ->
          is_dead_test_data.unsafe_g := Lval (var v);
          SkipChildren
      | _ -> SkipChildren


  (* Snarf the states of interest *)
  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = is_dead_test_data.label_one -> 
             is_dead_test_data.stmt_one := s
         | Label (name, _, _) -> ignore (printf "Failed for label name: %s\n" name)
         | _ -> ignore (printf "Skipping statement %a\n" d_stmt s)
      )
      s.labels;
    SkipChildren
end


(* Generate  data for the tests *)
let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;


(* Tests!!! *)
let test_isDead_safe_a = 
  let test = ID.is_dead !(is_dead_test_data.safe_a) !(is_dead_test_data.stmt_one) in
    TestCase(fun _ -> assert_bool "safe_a" test) 
;;


let test_isDead_safe_b = 
  let test = ID.is_dead !(is_dead_test_data.safe_b) !(is_dead_test_data.stmt_one) in
    TestCase(fun _ -> assert_bool "safe_b" test) 
;;


let test_isDead_unsafe_a = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_a) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_a" test) 
;;


let test_isDead_unsafe_b = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_b) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_b" test) 
;;


let test_isDead_unsafe_c = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_c) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_c" test) 
;;


let test_isDead_unsafe_d = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_d) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_d" test) 
;;


let test_isDead_unsafe_e = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_e) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_e" test) 
;;


let test_isDead_unsafe_f = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_f) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_f" test) 
;;


let test_isDead_unsafe_g = 
  let test = not (ID.is_dead !(is_dead_test_data.unsafe_g) !(is_dead_test_data.stmt_one)) in
    TestCase(fun _ -> assert_bool "unsafe_g" test) 
;;




(* Run all the tests *)
let suite_is_dead = 
  TestLabel ("IsDead", 
             TestList [
               TestLabel ("safe_a", test_isDead_safe_a);
               TestLabel ("safe_b", test_isDead_safe_b);
               TestLabel ("unsafe_a", test_isDead_unsafe_a);
               TestLabel ("unsafe_b", test_isDead_unsafe_b);
               TestLabel ("unsafe_c", test_isDead_unsafe_c);
               TestLabel ("unsafe_d", test_isDead_unsafe_d);
               TestLabel ("unsafe_e", test_isDead_unsafe_e);
               TestLabel ("unsafe_f", test_isDead_unsafe_f);
               TestLabel ("unsafe_g", test_isDead_unsafe_g);
             ]
  )
;;

let main = run_test_tt suite_is_dead

