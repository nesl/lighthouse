open OUnit
open AddAnnotations
  
(* Read annotations from a file into a hash table *)
let test_addAnnotations_getAnnotations = 
  let annotations = AddAnnotations.getAnnotations () in
    TestCase(fun _ -> assert_equal (Hashtbl.length annotations) 20);;

(* Given a function generate a new function with annotations added *)
let test_addAnnotations_addAttributes = 
  TestCase(fun _ -> assert_equal "this" "fails");;

let suite_addAnnotations = 
  TestLabel ("Add Annotations", 
             TestList [
               TestLabel ("getAnnotations", test_addAnnotations_getAnnotations);
               TestLabel ("addAttributes", test_addAnnotations_addAttributes)
             ]
  );;

let main = run_test_tt suite_addAnnotations
