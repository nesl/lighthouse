open OUnit
open AddAnnotations

open Pretty
open Cil
  
(* Read annotations from a file into a hash table *)
let test_addAnnotations_getAnnotations = 
  let annotations = AddAnnotations.getAnnotations "config.txt" in
    TestCase(fun _ -> assert_equal (Hashtbl.length annotations) 20);;

(* Run all the tests *)
let suite_addAnnotations = 
  TestLabel ("Add Annotations", 
             TestList [
               TestLabel ("getAnnotations", test_addAnnotations_getAnnotations)
             ]
  );;

let main = run_test_tt suite_addAnnotations

