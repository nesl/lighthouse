open Cil
open Pretty

let verbose = ref false
let currentFunc = ref (emptyFunction "")
    
class fnCallGraphVisitor = object
  inherit nopCilVisitor
    
  method vfunc (f:fundec) =
    currentFunc := f;
    if !verbose then
      ignore(printf "\"%s:%s\" [label = \"%s:%s\"];\n" 
               f.svar.vdecl.file
               f.svar.vname 
               f.svar.vdecl.file
               f.svar.vname)
    else
      ignore(printf "%s [label = %s];\n" f.svar.vname f.svar.vname);
    DoChildren


  method vinst (i:instr) =

      begin  
        match i with
          | Call (_, Lval (Var v, NoOffset), _, _) ->
              if !verbose then
                ignore(printf "\"%s:%s\" -> \"%s:%s\";\n" 
                         !currentFunc.svar.vdecl.file
                         !currentFunc.svar.vname 
                         v.vdecl.file
                         v.vname)
              else
                ignore(printf "%s -> %s;\n" !currentFunc.svar.vname v.vname);
              ()
          | _ ->
              ()
      end;

      DoChildren
              
end


let openFile (what: string) (takeit: out_channel -> unit) (fl: string) = 
  (try takeit (open_out fl)
   with _ ->
     raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))
;;


let argDescr = [
  ("--verbose", Arg.Unit (fun _ -> verbose := true),
   "Include function names in variable listing");

    ];;


let doFile fn = 
  let f = Frontc.parse fn () in  
  let fcgVisitor = new fnCallGraphVisitor in
    visitCilFileSameGlobals fcgVisitor f;
;;


let mainFunction () =

  let usageMsg = "Usage: fnCallGraph" in
  
  let fileNames : string list ref = ref [] in
  
  let recordFile fname = 
    fileNames := fname :: (!fileNames) 
  in
  
    Arg.parse argDescr recordFile usageMsg;
    
    Cil.initCIL ();

    fileNames := List.rev !fileNames;
    
    ignore(printf "digraph fnCallGraph {\n");    
    List.iter doFile !fileNames;
    ignore(printf "}\n")
;;
    
    

mainFunction ();;
      



