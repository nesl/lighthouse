open OUnit
open Pretty
open Cil
open CilDriver
  
(* Read annotations from a file into a hash table *)
let test_cilDriver_makeCilFile = 
  let inputFile = "cilDriverUnit.c" in
  let cilFile = makeCilFile inputFile in
    TestCase(fun _ -> assert_equal (cilFile.fileName) inputFile);;

(* Run all the tests *)
let suite_cilDriver = 
  TestLabel ("CIL File Manipulation", 
             TestList [
               TestLabel ("makeCilFile", test_cilDriver_makeCilFile)
             ]
  );;

let main = run_test_tt_main suite_cilDriver

