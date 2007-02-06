open OUnit
open Pretty
open Cil
open CilDriver
  
(* Set up a file for running tests *)
let inputFile = "isDeadUnit03.cil.c";;
let cilFile = makeCilFile inputFile;;

(* Prepare the file for pointer analysis queries *)
let _ = Ptranal.analyze_file cilFile;;
let _ = Ptranal.compute_results true;;
                                      
let main = Ptranal.print_types ()

