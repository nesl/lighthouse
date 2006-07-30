open OUnit
open CilDriver
open Pretty
open Cil

module IE = IsEquivalent
module IH = Inthash

(* Set up a file for running tests *)
let inputFile = "unitIsEquivalent.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.feature.fd_doit cilFile);;

(* Describe what we are interested in examining within CIL *)
type test_data = {
  array_a : exp ref;
  array_b : exp ref;
  clone_a : exp ref;
  clone_b : exp ref;
  clone_ab : exp ref;
  label_one : string;
  label_two : string;
  label_three : string;
  label_four : string;
  label_five : string;
  mutable id_one : int;
  mutable id_two : int;
  mutable id_three : int;
  mutable id_four : int;
  mutable id_five : int;
};;


let equiv_test_data = {
  array_a = ref zero;
  array_b = ref zero;
  clone_a = ref zero;
  clone_b = ref zero;
  clone_ab = ref zero;
  label_one = "ONE";
  label_two = "TWO";
  label_three = "THREE";
  label_four = "FOUR";
  label_five = "FIVE";
  id_one = 0;
  id_two = 0;
  id_three = 0;
  id_four = 0;
  id_five = 0;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor

  (* Prepare equiv clone information *)
  method vfunc (f:fundec) =
    IE.dbg_equiv_i := false;
    IE.dbg_equiv_combine := false;
    IE.dbg_equiv_stmt_summary := false;
    IE.dbg_equiv_df := false;

    IE.generate_equiv f;
    DoChildren


  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
        "arrayA" ->
          equiv_test_data.array_a := Lval (var v);
          SkipChildren
      | "arrayB" ->
          equiv_test_data.array_b := Lval (var v);
          SkipChildren
      | "arrayCloneA" ->
          equiv_test_data.clone_a := Lval (var v);
          SkipChildren
      | "arrayCloneB" ->
          equiv_test_data.clone_b := Lval (var v);
          SkipChildren
      | "arrayCloneAB" ->
          equiv_test_data.clone_ab := Lval (var v);
          SkipChildren
      | _ -> SkipChildren


  (* Snarf the states of interest *)
  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = equiv_test_data.label_one -> 
             equiv_test_data.id_one <- s.sid
         | Label (name, _, _) when name = equiv_test_data.label_two -> 
             equiv_test_data.id_two <- s.sid
         | Label (name, _, _) when name = equiv_test_data.label_three -> 
             equiv_test_data.id_three <- s.sid
         | Label (name, _, _) when name = equiv_test_data.label_four -> 
             equiv_test_data.id_four <- s.sid
         | Label (name, _, _) when name = equiv_test_data.label_five -> 
             equiv_test_data.id_five <- s.sid
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


(* Tests!!! *)
let test_equivClone_one = 
  let a_dead = is_dead !(equiv_test_data.clone_a) equiv_test_data.id_one in
  let b_dead = is_dead !(equiv_test_data.clone_b) equiv_test_data.id_one in
  let cab_dead = is_dead !(equiv_test_data.clone_ab) equiv_test_data.id_one in
  TestCase(fun _ -> assert_bool "Incorrect clone information" (a_dead && b_dead && cab_dead)) 
;;



let test_equivClone_two = 
  let ca_to_a = 
    IE.is_equiv 
      !(equiv_test_data.clone_a) 
      !(equiv_test_data.array_a)
      equiv_test_data.id_two
  in
  let a_to_ca = 
    IE.is_equiv 
      !(equiv_test_data.array_a)
      !(equiv_test_data.clone_a) 
      equiv_test_data.id_two
  in
  let cb_to_b = 
    IE.is_equiv 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_two
  in
  let b_to_cb = 
    IE.is_equiv 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_b) 
      equiv_test_data.id_two
  in
  let cab_dead = is_dead !(equiv_test_data.clone_ab) equiv_test_data.id_two in
  TestCase(fun _ -> assert_bool "Incorrect clone information" 
                      (ca_to_a && a_to_ca && cb_to_b && b_to_cb && cab_dead))
;;



let test_equivClone_three = 
  let ca_to_a = 
    IE.is_equiv 
      !(equiv_test_data.clone_a) 
      !(equiv_test_data.array_a)
      equiv_test_data.id_three
  in
  let a_to_ca = 
    IE.is_equiv 
      !(equiv_test_data.array_a)
      !(equiv_test_data.clone_a) 
      equiv_test_data.id_three
  in
  let cb_to_b = 
    IE.is_equiv 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_three
  in
  let b_to_cb = 
    IE.is_equiv 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_b) 
      equiv_test_data.id_three
  in
  let cab_to_b = 
    IE.is_equiv 
      !(equiv_test_data.clone_ab) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_three
  in
  let b_to_cab = 
    IE.is_equiv 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_ab) 
      equiv_test_data.id_three
  in
  let cb_to_cab = 
    IE.is_equiv 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.clone_ab)
      equiv_test_data.id_three
  in
  let cab_to_cb = 
    IE.is_equiv 
      !(equiv_test_data.clone_ab)
      !(equiv_test_data.clone_b) 
      equiv_test_data.id_three
  in
  TestCase(fun _ -> assert_bool "Incorrect clone information" 
                      (ca_to_a && a_to_ca && cb_to_b && b_to_cb && 
                       cab_to_b && b_to_cab && cb_to_cab && cab_to_cb))
;;



let test_equivClone_four = 
  let ca_to_a = 
    IE.is_equiv 
      !(equiv_test_data.clone_a) 
      !(equiv_test_data.array_a)
      equiv_test_data.id_four
  in
  let a_to_ca = 
    IE.is_equiv 
      !(equiv_test_data.array_a)
      !(equiv_test_data.clone_a) 
      equiv_test_data.id_four
  in
  let cb_to_b = 
    IE.is_equiv 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_four
  in
  let b_to_cb = 
    IE.is_equiv 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_b) 
      equiv_test_data.id_four
  in
  let cab_dead = is_dead !(equiv_test_data.clone_ab) equiv_test_data.id_four in
  TestCase(fun _ -> assert_bool "Incorrect clone information" 
                      (ca_to_a && a_to_ca && cb_to_b && b_to_cb && cab_dead))
;;



let test_equivClone_five = 
  let a_good = 
    IE.is_equiv 
      !(equiv_test_data.clone_a) 
      IE.nullPtr
      equiv_test_data.id_five
  in
  let b_good = 
    IE.is_equiv
      !(equiv_test_data.clone_b) 
      IE.nullPtr
      equiv_test_data.id_five
  in
  let ab_good = 
    IE.is_equiv
      !(equiv_test_data.clone_ab) 
      IE.nullPtr
      equiv_test_data.id_five
  in
  TestCase(fun _ -> assert_bool "Incorrect clone information" (a_good && b_good && ab_good)) 
;;



(* Run all the tests *)
let suite_equivClone = 
  TestLabel ("Add Annotations", 
             TestList [
               TestLabel ("equivClone.c: One", test_equivClone_one);
               TestLabel ("equivClone.c: Two", test_equivClone_two);
               TestLabel ("equivClone.c: Three", test_equivClone_three);
               TestLabel ("equivClone.c: Four", test_equivClone_four);
               TestLabel ("equivClone.c: Five", test_equivClone_five);
             ]
  )
;;

let main = run_test_tt suite_equivClone

