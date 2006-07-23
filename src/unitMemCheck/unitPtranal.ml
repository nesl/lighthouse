open OUnit
open Pretty
open Cil
open CilDriver
  
(* Set up a file for running tests *)
let inputFile = "unitPtranal.c";;
let cilFile = makeCilFile inputFile;;

(* Prepare the file for pointer analysis queries *)
let _ = Ptranal.analyze_file cilFile;;
let _ = Ptranal.compute_results true;;
                                      
let main = Ptranal.print_types ()

(*
let test_cilDriver_makeCilFile = 
  let inputFile = "foo.c" in
  let cilFile = makeCilFile inputFile in
    TestCase(fun _ -> assert_equal (cilFile.fileName) inputFile);;

let suite_ptranal = 
  TestLabel ("CIL File Manipulation", 
             TestList [
               TestLabel ("makeCilFile", test_cilDriver_makeCilFile)
             ]
  );;

let main = run_test_tt_main suite_cilDriver
 *)
