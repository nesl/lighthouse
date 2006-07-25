open OUnit
open CilDriver
open Pretty
open Cil

module MA = MustAlias
module IH = Inthash


(* Set up a file for running tests *)
let inputFile = "unitMustAlias.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.feature.fd_doit cilFile);;

(* Describe what we are interested in examining within CIL *)
type test_data = {
  array_a : exp ref;
  array_b : exp ref;
  alias_a : exp ref;
  alias_b : exp ref;
  alias_ab : exp ref;
  label_one : string;
  label_two : string;
  label_three : string;
  label_four : string;
  label_five : string;
  label_six : string;
  label_seven : string;
  mutable id_one : int;
  mutable id_two : int;
  mutable id_three : int;
  mutable id_four : int;
  mutable id_five : int;
  mutable id_six : int;
  mutable id_seven : int;
};;


let must_test_data = {
  array_a = ref zero;
  array_b = ref zero;
  alias_a = ref zero;
  alias_b = ref zero;
  alias_ab = ref zero;
  label_one = "ONE";
  label_two = "TWO";
  label_three = "THREE";
  label_four = "FOUR";
  label_five = "FIVE";
  label_six = "SIX";
  label_seven = "SEVEN";
  id_one = 0;
  id_two = 0;
  id_three = 0;
  id_four = 0;
  id_five = 0;
  id_six = 0;
  id_seven = 0;
};;



(* Create a visitor to examine aliases *)
class testVisitor = object
  inherit nopCilVisitor

  (* Prepare must alias information *)
  method vfunc (f:fundec) =
    MA.generate_must_alias f;
    DoChildren


  (* Snarf the variables of interest *)
  method vvdec (v:varinfo) =
    match v.vname with
        "arrayA" ->
          must_test_data.array_a := Lval (var v);
          SkipChildren
      | "arrayB" ->
          must_test_data.array_b := Lval (var v);
          SkipChildren
      | "arrayAliasA" ->
          must_test_data.alias_a := Lval (var v);
          SkipChildren
      | "arrayAliasB" ->
          must_test_data.alias_b := Lval (var v);
          SkipChildren
      | "arrayAliasAB" ->
          must_test_data.alias_ab := Lval (var v);
          SkipChildren
      | _ -> SkipChildren


  (* Snarf the states of interest *)
  method vstmt (s:stmt) =
    List.iter 
      (fun l -> match l with 
           Label (name, _, _) when name = must_test_data.label_one -> 
             must_test_data.id_one <- s.sid
         | Label (name, _, _) when name = must_test_data.label_two -> 
             must_test_data.id_two <- s.sid
         | Label (name, _, _) when name = must_test_data.label_three -> 
             must_test_data.id_three <- s.sid
         | Label (name, _, _) when name = must_test_data.label_four -> 
             must_test_data.id_four <- s.sid
         | Label (name, _, _) when name = must_test_data.label_five -> 
             must_test_data.id_five <- s.sid
         | Label (name, _, _) when name = must_test_data.label_six -> 
             must_test_data.id_six <- s.sid
         | Label (name, _, _) when name = must_test_data.label_seven -> 
             must_test_data.id_seven <- s.sid
         | Label (name, _, _) -> ignore (printf "Failed for label name: %s\n" name)
         | _ -> ignore (printf "Skipping statement %a\n" d_stmt s)
                                 
      )
      s.labels;
    SkipChildren
end

(* Generate  data for the tests *)
let tv = new testVisitor;;
visitCilFileSameGlobals tv cilFile;;

(* Helper function to check that an expression is considerd Dead by the alias
 * analysis *)
let is_dead (e:exp) (id:int) : bool =
  match MA.get_alias e id with
      MA.Dead -> true
    | _ -> false
;;


(* Tests!!! *)
let test_mustAlias_one = 
  let a_good = is_dead !(must_test_data.alias_a) must_test_data.id_one in
  let b_good = is_dead !(must_test_data.alias_b) must_test_data.id_one in
  let ab_good = is_dead !(must_test_data.alias_ab) must_test_data.id_one in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;



let test_mustAlias_two = 
  let a_good = 
    MA.must_alias 
      !(must_test_data.alias_a) 
      !(must_test_data.array_a)
      must_test_data.id_two
  in
  let b_good = is_dead !(must_test_data.alias_b) must_test_data.id_two in
  let ab_good = is_dead !(must_test_data.alias_ab) must_test_data.id_two in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;



let test_mustAlias_three = 
  let a_good = 
    MA.must_alias
      !(must_test_data.alias_a) 
      !(must_test_data.array_a)
      must_test_data.id_three
  in
  let b_good = is_dead !(must_test_data.alias_b) must_test_data.id_three in
  let ab_good = is_dead !(must_test_data.alias_ab) must_test_data.id_three in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;



let test_mustAlias_four = 
  let a_good = 
    MA.must_alias 
      !(must_test_data.alias_a) 
      !(must_test_data.array_a)
      must_test_data.id_four
  in
  let b_good = 
    MA.must_alias
      !(must_test_data.alias_b) 
      !(must_test_data.array_b)
      must_test_data.id_four
  in
  let ab_good = is_dead !(must_test_data.alias_ab) must_test_data.id_four in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;



let test_mustAlias_five = 
  let a_good = 
    MA.must_alias 
      !(must_test_data.alias_a) 
      !(must_test_data.array_a)
      must_test_data.id_five
  in
  let b_good = 
    MA.must_alias
      !(must_test_data.alias_b) 
      !(must_test_data.array_b)
      must_test_data.id_five
  in
  let ab_good = 
    MA.must_alias
      !(must_test_data.alias_ab) 
      !(must_test_data.array_a)
      must_test_data.id_five
  in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;



let test_mustAlias_six = 
  let a_good = 
    MA.must_alias 
      !(must_test_data.alias_a) 
      !(must_test_data.array_a)
      must_test_data.id_six
  in
  let b_good = 
    MA.must_alias
      !(must_test_data.alias_b) 
      !(must_test_data.array_b)
      must_test_data.id_six
  in
  let ab_good = 
    MA.must_alias
      !(must_test_data.alias_ab) 
      !(must_test_data.array_b)
      must_test_data.id_six
  in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;



let test_mustAlias_seven = 
  let a_good = 
    MA.must_alias 
      !(must_test_data.alias_a) 
      !(must_test_data.array_a)
      must_test_data.id_seven
  in
  let b_good = 
    MA.must_alias
      !(must_test_data.alias_b) 
      !(must_test_data.array_b)
      must_test_data.id_seven
  in
  let ab_good = is_dead !(must_test_data.alias_ab) must_test_data.id_seven in
  TestCase(fun _ -> assert_bool "Incorrect alias information" (a_good && b_good && ab_good)) 
;;


(* Run all the tests *)
let suite_mustAlias = 
  TestLabel ("Add Annotations", 
             TestList [
               TestLabel ("mustAlias.c: One", test_mustAlias_one);
               TestLabel ("mustAlias.c: Two", test_mustAlias_two);
               TestLabel ("mustAlias.c: Three", test_mustAlias_three);
               TestLabel ("mustAlias.c: Four", test_mustAlias_four);
               TestLabel ("mustAlias.c: Five", test_mustAlias_five);
               TestLabel ("mustAlias.c: Six", test_mustAlias_six);
               TestLabel ("mustAlias.c: Seven", test_mustAlias_seven);
             ]
  )
;;

let main = run_test_tt suite_mustAlias

