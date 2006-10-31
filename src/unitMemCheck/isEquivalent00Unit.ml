open OUnit
open CilDriver
open Pretty
open Cil

module IE = IsEquivalent;;

IE.dbg_is_equiv_i := false;;
IE.dbg_is_equiv_c := false;;
IE.dbg_is_equiv_stmt_summary := false;;
IE.dbg_is_equiv_get_aliases := false;;
IE.dbg_is_equiv_get_equiv_set := false;;
IE.dbg_is_equiv_df := false;;

(* Set up a file for running tests *)
let inputFile = "isEquivalent00Unit.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.make_one_cfg cilFile);;

(* Describe what we are interested in examining within CIL *)
type test_data = {
  array_a : exp ref;
  array_b : exp ref;
  clone_a : exp ref;
  clone_b : exp ref;
  clone_ab : exp ref;
  p : exp ref;
  q : exp ref;
  x : exp ref;
  y : exp ref;
  mutable id_one : int;
  mutable id_two : int;
  mutable id_three : int;
  mutable id_four : int;
  mutable id_five : int;
  mutable id_six : int;
};;


let equiv_test_data = {
  array_a = ref zero;
  array_b = ref zero;
  clone_a = ref zero;
  clone_b = ref zero;
  clone_ab = ref zero;
  p = ref zero;
  q = ref zero;
  x = ref zero;
  y = ref zero;
  id_one = 0;
  id_two = 0;
  id_three = 0;
  id_four = 0;
  id_five = 0;
  id_six = 0;
};;



(* Create a visitor to examine clones *)
class testVisitor = object
  inherit nopCilVisitor

  (* Prepare equiv clone information *)
  method vfunc (f:fundec) =
    
    IE.generate_equiv f cilFile;
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
      | "p" ->
          equiv_test_data.p := Lval (var v);
          SkipChildren
      | "q" ->
          equiv_test_data.q := Lval (var v);
          SkipChildren
      | "x" ->
          equiv_test_data.x := Lval (var v);
          SkipChildren
      | "y" ->
          equiv_test_data.y := Lval (var v);
          SkipChildren
      | _ -> SkipChildren


  (* Snarf the states of interest *)
  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = "ONE" -> 
             equiv_test_data.id_one <- s.sid
         | Label (name, _, _) when name = "TWO" -> 
             equiv_test_data.id_two <- s.sid
         | Label (name, _, _) when name = "THREE" -> 
             equiv_test_data.id_three <- s.sid
         | Label (name, _, _) when name = "FOUR" -> 
             equiv_test_data.id_four <- s.sid
         | Label (name, _, _) when name = "FIVE" -> 
             equiv_test_data.id_five <- s.sid
         | Label (name, _, _) when name = "SIX" -> 
             equiv_test_data.id_six <- s.sid
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
  match IE.get_equiv_set_end e id with
      [e] -> true
    | [] -> true
    | el -> 
        List.iter (fun e -> ignore (printf "*** %a\n" d_exp e)) el;
        false
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
    IE.is_equiv_end 
      !(equiv_test_data.clone_a) 
      !(equiv_test_data.array_a)
      equiv_test_data.id_two
  in
  let a_to_ca = 
    IE.is_equiv_end 
      !(equiv_test_data.array_a)
      !(equiv_test_data.clone_a) 
      equiv_test_data.id_two
  in
  let cb_to_b = 
    IE.is_equiv_end 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_two
  in
  let b_to_cb = 
    IE.is_equiv_end 
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
    IE.is_equiv_end 
      !(equiv_test_data.clone_a) 
      !(equiv_test_data.array_a)
      equiv_test_data.id_three
  in
  let a_to_ca = 
    IE.is_equiv_end 
      !(equiv_test_data.array_a)
      !(equiv_test_data.clone_a) 
      equiv_test_data.id_three
  in
  let cb_to_b = 
    IE.is_equiv_end 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_three
  in
  let b_to_cb = 
    IE.is_equiv_end 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_b) 
      equiv_test_data.id_three
  in
  let cab_to_b = 
    IE.is_equiv_end 
      !(equiv_test_data.clone_ab) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_three
  in
  let b_to_cab = 
    IE.is_equiv_end 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_ab) 
      equiv_test_data.id_three
  in
  let cb_to_cab = 
    IE.is_equiv_end 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.clone_ab)
      equiv_test_data.id_three
  in
  let cab_to_cb = 
    IE.is_equiv_end 
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
    IE.is_equiv_end 
      !(equiv_test_data.clone_a) 
      !(equiv_test_data.array_a)
      equiv_test_data.id_four
  in
  let a_to_ca = 
    IE.is_equiv_end 
      !(equiv_test_data.array_a)
      !(equiv_test_data.clone_a) 
      equiv_test_data.id_four
  in
  let cb_to_b = 
    IE.is_equiv_end 
      !(equiv_test_data.clone_b) 
      !(equiv_test_data.array_b)
      equiv_test_data.id_four
  in
  let b_to_cb = 
    IE.is_equiv_end 
      !(equiv_test_data.array_b)
      !(equiv_test_data.clone_b) 
      equiv_test_data.id_four
  in
  let cab_dead = is_dead !(equiv_test_data.clone_ab) equiv_test_data.id_four in
  TestCase(fun _ -> assert_bool "Incorrect clone information" 
                      (ca_to_a && a_to_ca && cb_to_b && b_to_cb && cab_dead))
;;


let test_equivClone_five_a = 
  let a_good = 
    IE.is_equiv_end 
      !(equiv_test_data.clone_a) 
      IE.nullPtr
      equiv_test_data.id_five
  in
    TestCase(fun _ -> assert_bool "a should be Null" a_good) 
;;


let test_equivClone_five_b = 
  let b_good = 
    IE.is_equiv_end
      !(equiv_test_data.clone_b) 
      IE.nullPtr
      equiv_test_data.id_five
  in
  TestCase(fun _ -> assert_bool "b should be Null" b_good) 
;;


let test_equivClone_five_ab = 
  let ab_good = 
    IE.is_equiv_end
      !(equiv_test_data.clone_ab) 
      IE.nullPtr
      equiv_test_data.id_five
  in
  TestCase(fun _ -> assert_bool "ab should be Null" ab_good) 
;;


let test_equivClone_six = 

  let star (e:exp) : exp =
    (Lval (mkMem e NoOffset))
  in
  
  let x_to_y = 
    IE.is_equiv_end 
      !(equiv_test_data.x) 
      !(equiv_test_data.y) 
      equiv_test_data.id_six
  in
    
  let not_sx_to_p = 
    not (
      IE.is_equiv_end
        (star !(equiv_test_data.x))
        !(equiv_test_data.p) 
        equiv_test_data.id_six
    )
  in

  let sy_to_q =
    IE.is_equiv_end
      (star !(equiv_test_data.y))
      !(equiv_test_data.q)
      equiv_test_data.id_six
  in

  let sx_to_sy =
    IE.is_equiv_end
      (star !(equiv_test_data.x))
      (star !(equiv_test_data.y))
      equiv_test_data.id_six
  in
  
  let sx_to_q =
    IE.is_equiv_end
      (star !(equiv_test_data.x))
      !(equiv_test_data.q)
      equiv_test_data.id_six
  in

    TestCase(fun _ -> assert_bool "Incorrect clone information" 
                      (x_to_y && not_sx_to_p && sy_to_q && sx_to_sy && sx_to_q)) 
;;

(* Run all the tests *)
let suite_equivClone = 
  TestLabel ("IsEquivalent", 
             TestList [
               TestLabel ("ONE", test_equivClone_one);
               TestLabel ("TWO", test_equivClone_two);
               TestLabel ("THREE", test_equivClone_three);
               TestLabel ("FOUR", test_equivClone_four);
               TestLabel ("FIVE", test_equivClone_five_a);
               TestLabel ("FIVE", test_equivClone_five_b);
               TestLabel ("FIVE", test_equivClone_five_ab);
               TestLabel ("SIX", test_equivClone_six);
             ]
  )
;;

let main = run_test_tt suite_equivClone;;

