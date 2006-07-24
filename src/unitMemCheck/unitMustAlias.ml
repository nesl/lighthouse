open OUnit
open CilDriver
open Pretty
open Cil

module MF = MustFlow
module IH = Inthash

  
(* Set up a file for running tests *)
let inputFile = "unitMustAlias.c";;
let cilFile = makeCilFile inputFile;;
ignore (MakeOneCFG.feature.fd_doit cilFile);;
  
(* Debugging *)
MF.dbg_must_i := true;;
MF.dbg_must_combine := true;;
                  
(* Section of code that we wish to examine and references to expressions of
 * interest *)
let testLabel = ref "ZERO";;
let aliasA = ref zero;;
let aliasB = ref zero;;
let aliasAB = ref zero;;


(* Create a visitor to examine aliases *)
class testVisitor = object
  inherit nopCilVisitor

  (* Generate function's must alias information *)
  method vfunc (f:fundec) =
    IH.clear MF.DFM.stmtStartData;
    IH.add MF.DFM.stmtStartData (List.hd f.sbody.bstmts).sid (Hashtbl.create 5);
    MF.TrackF.compute [(List.hd f.sbody.bstmts)];
    DoChildren

    
  (* Snarf the variables of interest into references *)
  method vvdec (v:varinfo) =
    match v.vname with
        "arrayAliasA" ->
          aliasA := Lval (var v);
          SkipChildren
      | "arrayAliasB" ->
          aliasB := Lval (var v);
          SkipChildren
      | "arrayAliasAB" ->
          aliasAB := Lval (var v);
          SkipChildren
      | _ -> SkipChildren

  (* Assume that caller has set the referance "testLabel".  This visitor will
   * find the statement with this label and then traverse its children. *)
  method vstmt (s:stmt) =
    let found_label =
      List.exists 
        (fun l -> match l with 
             Label (name, loc, b) when name = !testLabel -> true
           | _ -> false
        ) 
        s.labels;
    in
      if found_label then (
        ignore (printf "\n\nFound label %s\n" !testLabel);
        ignore (printf "Must Alias information at %a\n" d_stmt s);
        match (MF.getStmtState MF.DFM.stmtStartData s) with
          
            Some table -> 
              Hashtbl.iter 
                (fun key value -> 
                   ignore (printf "%s -> " key.vname);
                   match value with
                       MF.Next v -> ignore (printf "%s\n" v.vname)
                     | MF.End -> ignore (printf "End\n")
                     | MF.Null -> ignore (printf "Null\n")
                )
                table;
              SkipChildren
          
          
          | None -> 
              SkipChildren
      ) else (
        SkipChildren
      )

  (* Print the instruction *)
  method vinst (i:instr) =
    SkipChildren
      

end
        

let test_mustAlias_one = 
  testLabel := "ONE";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
    TestCase(fun _ -> assert_failure "Test not implemented") 
;;

let test_mustAlias_two = 
  testLabel := "TWO";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
  TestCase(fun _ -> assert_failure "Test not implemented") 
;;

let test_mustAlias_three = 
  testLabel := "THREE";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
  TestCase(fun _ -> assert_failure "Test not implemented") 
;;

let test_mustAlias_four = 
  testLabel := "FOUR";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
  TestCase(fun _ -> assert_failure "Test not implemented") 
;;

let test_mustAlias_five = 
  testLabel := "FIVE";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
  TestCase(fun _ -> assert_failure "Test not implemented") 
;;

let test_mustAlias_six = 
  testLabel := "SIX";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
  TestCase(fun _ -> assert_failure "Test not implemented") 
;;

let test_mustAlias_seven = 
  testLabel := "SEVEN";
  let tv = new testVisitor in
    visitCilFileSameGlobals tv cilFile;
  TestCase(fun _ -> assert_failure "Test not implemented") 
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

