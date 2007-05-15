(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

(* Specification file *)
let spec_file = ref "";;
let fsm_file = ref "";;

(* Function to check *)
let func_name = ref "";;

(** [cil_file] CIL file being analyzed. *)
let cil_file: file ref = ref dummyFile;;

(** Flag to enable checking of functions defined in included header files. These
  * are mainly macros that cause a lot of extra runtime overhead. *)
let check_headers = ref false;;


(** Visitor *)
class rocketVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =
      
    let fsm_graph file_name = 
      Dot2transCheck.get_fsm_graph file_name
    in

    let simple_check () =
      IsEquivalent.generate_equiv f !cil_file;
      try 
        ignore (Apollo.apollo_func_simple f !cil_file);
      with 
          E.Error -> ignore (printf "####\n# Bummer!\n####\n\n\n");
    in


    let fsm_check edge =
     
      let (from_node, to_node, spec_string) = edge in
      
      let _ = IsEquivalent.generate_equiv f !cil_file in
      let (spec, _) = SpecParse.parse_spec_string spec_string in
      let _ = State.fsm_specification := spec in

      try 
        ignore (Apollo.apollo_func_fsm f !cil_file);
      with 
          E.Error -> ignore (printf "####\n# Bummer!\n####\n\n\n");
    in


      if not (!fsm_file = "") then (
      (** Checking using FSM takes priority *)
        let graph = fsm_graph !fsm_file in

        let matching_edges = 
          List.filter 
            (fun (_, to_node, _) -> to_node = f.svar.vname) 
            graph
        in

          List.iter fsm_check matching_edges

      ) else if !func_name = f.svar.vname then (
      (** Then function specific check requests take priority *)
        simple_check()

      ) else if ((!func_name = "") && (!check_headers)) then (
      (** Then either check all function *)
        simple_check()

      ) else if 
        ((!func_name = "") && 
         (not !check_headers) &&
         ((String.get !currentLoc.file 
                  ((String.length !currentLoc.file) -1)) = 'c') && 
         ((String.get !currentLoc.file 
                  ((String.length !currentLoc.file) -2)) = '.'))
      then (
      (** or skip over functions defined in header files *)
        simple_check ()
      ) else (
        ()
      );
        
      DoChildren

end

(** {1 Utility and Driver Functions} *)

(** {2 File Output} *)

(** Utility function used to open a file for writing the transformed program. *)
let openFile (what: string) (takeit: out_channel -> unit) (fl: string) = 
  if !E.verboseFlag then
    ignore (Printf.printf "Setting %s to %s\n" what fl);
  (try takeit (open_out fl)
   with _ ->
     raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))
;;


(** Reference to the channel that will be used to output the transformed code to
  * file. *)
let outChannel : out_channel option ref = ref None;;


(** {2 Command Line Options} *)

(** Descrimption of the command line interface to Lighthouse. *)
let argDescr = [
  
  ("--spec", Arg.String (fun spec -> spec_file := spec),
   "Use specification file for pre- / post- condition specifications");
  
  ("--fsm", Arg.String (fun name -> fsm_file := name),
   "Use finite state machine specification to drive analysis");

  ("--func_name", Arg.String (fun name -> func_name := name),
   "Name of function to verify (default is all functions)");

  ("--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
   "Name of the output CIL file");
  
  ("--check_headers", Arg.Unit (fun _ -> check_headers := true),
   "Enable checking of header files");

  ("--strict", Arg.Unit (fun _ -> Apollo.strict := true),
   "Use strict checking that assumes non-speficied stores to be off limits");
  
  ("--dbg_state", Arg.Unit (fun _ -> State.dbg_state := true),
   "Debug state tracking");

  (* Configuration options *)

  ("--dbg_apollo_s", Arg.Unit (fun _ -> Apollo.dbg_apollo_s := true),
   "Statement level debugging of the Apollo dataflow");

  ("--dbg_apollo_i", Arg.Unit (fun _ -> Apollo.dbg_apollo_i := true),
   "Instruction level debugging of the Apollo dataflow");

  ("--dbg_apollo_c", Arg.Unit (fun _ -> Apollo.dbg_apollo_c := true),
   "Join debugging of the Apollo dataflow");

  ("--dbg_apollo_g", Arg.Unit (fun _ -> Apollo.dbg_apollo_g := true),
   "Guard evaluation debugging of the Apollo dataflow");

  (* IsEquivalent specific debugging *)
  
  ("--dbg_is_equiv_i", Arg.Unit (fun _ -> IsEquivalent.dbg_is_equiv_i := true),
   "Instruction level debugging of the IsEquivalent dataflow");

  ("--dbg_is_equiv_c", Arg.Unit (fun _ -> IsEquivalent.dbg_is_equiv_c := true),
   "Join debugging of the IsEquivalent dataflow");

  ("--dbg_is_equiv_stmt_summary", Arg.Unit (fun _ -> IsEquivalent.dbg_is_equiv_stmt_summary := true),
   "Dump summary of incoming statement equivalency sets generated by IsEquivalent dataflow");

  ("--dbg_is_equiv_get_aliases", Arg.Unit (fun _ -> IsEquivalent.dbg_is_equiv_get_aliases := true),
   "Dump the result of calls to get_aliases within the IsEquivalent dataflow");
  
  ("--dbg_is_equiv_get_equiv_set", Arg.Unit (fun _ -> IsEquivalent.dbg_is_equiv_get_equiv_set := true),
   "Dump the result of calles to get_equiv_set within the Isequivalent dataflow");

];;


(** {2 Process a File} *)

let doFile (file_name: string) : unit = 

  cil_file := Frontc.parse file_name ();

  (* Execute other modules in the correct order *) 
  ignore (Simplemem.feature.fd_doit !cil_file);
  ignore (Simplify.feature.fd_doit !cil_file);
  ignore (MakeOneCFG.make_one_cfg !cil_file);
  ignore (Ptranal.feature.fd_doit !cil_file);
  
  (* If requested dump the transformed code to file. *)
  (match !outChannel with
       None -> ()
     | Some c -> Stats.time "printCIL" 
                   (dumpFile (!printerForMaincil) c !cil_file.fileName) !cil_file);

  (* Visit! *)
  visitCilFileSameGlobals (new rocketVisitor) !cil_file;

  ()
;;


(** {2 Read Command Line} *)

(** Read and process the command line, and then run Lighthouse on the requested
  * file. *)
let mainFunction () =

  let usageMsg = "Usage: rocket --spec <spec_file> [options] source-file" in
  
  let fileNames : string list ref = ref [] in
  
  let recordFile fname = 
    fileNames := fname :: (!fileNames) 
  in
  
    Arg.parse argDescr recordFile usageMsg;
    
    Cil.initCIL ();

    fileNames := List.rev !fileNames;
    
   
    if not (List.length !fileNames == 1) then (
      Arg.usage argDescr usageMsg;
      exit 0;
    );

    let (spec, (alloc_funcs, free_funcs)) = 
      if (!spec_file = "") then (
        ({State.stores=[]; State.pre=[]; State.post=[]}, ([], []))
      ) else (
        SpecParse.parse_spec_file !spec_file 
      )
    in
    
      State.specification := spec;
      IsEquivalent.alloc_funcs := alloc_funcs;
      IsEquivalent.free_funcs := free_funcs;
      
      List.iter doFile !fileNames;
;;
    
    
(* Do stuff *)
mainFunction ();;
      



