open OUnit
open AddAnnotations
open CilDriver

open Pretty
open Cil
  
(* Read annotations from a file into a hash table *)
let test_addAnnotations_getAnnotations = 
  let annotations = AddAnnotations.getAnnotations "addAnnotationsUnit.txt" in
    TestCase(fun _ -> assert_equal 
                        ~msg:"Incorrect number of attribute mappings loaded"
                        (Hashtbl.length annotations) 2)
;;
  

(* Add attributes to the createIntArray function *)
let test_addAnnotations_addFunAttributes_decl =

  let annotations = AddAnnotations.getAnnotations "addAnnotationsUnit.txt" in
  let inputFile = "addAnnotationsUnit.c" in
  let cilFile = makeCilFile inputFile in
  let attributes = Hashtbl.find annotations "createIntArray" in

  let funDecl = 
    List.hd (
      foldGlobals cilFile 
        (fun l glob -> match glob with
             GVarDecl (v, _) when v.vname = "createIntArray" -> [v]
           | _ -> l
        )
        []
    )
  in

  let updatedReturnType = match (addFunAttributes funDecl attributes) with
      TFun(t, _, _, _) -> hasAttribute "sos_claim" (typeAttrs t)
    | _ -> false
  in

    TestCase(fun _ -> assert_bool "Failed to set declaration attribute" updatedReturnType)
;;

(* Add attributes to the createIntArray function *)
let test_addAnnotations_addFunAttributes_def =

  let annotations = AddAnnotations.getAnnotations "addAnnotationsUnit.txt" in
  let inputFile = "addAnnotationsUnit.c" in
  let cilFile = makeCilFile inputFile in
  let attributes = Hashtbl.find annotations "createIntArray" in

  let funDef = 
    List.hd (
      foldGlobals cilFile 
        (fun l glob -> match glob with
             GFun (f, _) when f.svar.vname = "createIntArray" ->  [f.svar]
           | _ -> l
        )
        []
    )
  in

  let updatedReturnType = match (addFunAttributes funDef attributes) with
      TFun(t, _, _, _) -> hasAttribute "sos_claim" (typeAttrs t)
    | _ -> false
  in

    TestCase(fun _ -> assert_bool "Failed to set definition attribute" updatedReturnType)
;;

(* Run all the tests *)
let suite_addAnnotations = 
  TestLabel ("Add Annotations", 
             TestList [
               TestLabel ("getAnnotations", test_addAnnotations_getAnnotations);
               TestLabel ("addFunAttributes_decl", test_addAnnotations_addFunAttributes_decl);
               TestLabel ("addFunAttributes_def", test_addAnnotations_addFunAttributes_def)
             ]
  )
;;

let main = run_test_tt suite_addAnnotations

