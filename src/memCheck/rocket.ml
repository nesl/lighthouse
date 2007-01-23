(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

(* Specification file *)
let spec_file = ref "";;
let specification = ref [];;

(** [cil_file] CIL file being analyzed. *)
let cil_file: file ref = ref dummyFile;;

(** Flag to enable checking of functions defined in included header files. These
  * are mainly macros that cause a lot of extra runtime overhead. *)
let check_headers = ref false;;


(** Helper functions *)
let get_global_stores (f: file): Apollo.store_data list = 
  foldGlobals 
    f
    (fun s g -> match g with
         GVarDecl (v, l) 
       | GVar (v, _, l) when not (isFunctionType v.vtype) -> 
           (Apollo.Error, (Lval (var v)))::s
       | GFun (fd, l) -> s
       | _ -> s
    ) 
    []
;;


let get_local_stores (f: fundec): Apollo.store_data list = 
  let stores = 
    List.map 
      (fun v -> (Apollo.Error, (Lval (var v))))
      (List.filter 
         (fun v -> (hasAttribute "sos_store" v.vattr)) 
         (f.slocals @ f.sformals)
      )
  in

  let must_claim =
    List.map 
      (fun v -> (Apollo.Empty, (Lval (var v))))
      (List.filter 
         (fun v -> (hasAttribute "sos_claim" v.vattr)) 
         f.sformals
      )
  in

    stores @ must_claim
;;
        

let get_local_heaps (f: fundec): Apollo.heap_data list = 
  let heap_vars = 
    List.filter 
      (fun v -> (hasAttribute "sos_release" v.vattr)) 
      f.sformals
  in
    List.map (fun v -> (Lval (var v))) heap_vars
;;


let set_pre stores heaps full empty =

  let match_str_exp s e =
    match e with
        Lval (Var v, _) -> v.vname = s
      | _ -> E.s (E.bug "Unable to match against expression %a" d_exp e)
  in


  let update_state states = 
    List.fold_left
      (fun new_stores (s_state, s_var) ->
         if List.exists (fun s -> match_str_exp s s_var) full then
           (Apollo.Full, s_var)::new_stores
         else if List.exists (fun s -> match_str_exp s s_var) empty then
           (Apollo.Empty, s_var)::new_stores
         else
           (s_state, s_var)::new_stores
      )
      []
      states
  in

    (update_state stores, heaps)
;;

(** Visitor *)
class rocketVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =

    (** Check to see if we can skip this function since it may be defined in a
      * header file *)
      
    if (not !check_headers) &&
       (String.get !currentLoc.file ((String.length !currentLoc.file) -1)) = 'h' &&
       (String.get !currentLoc.file ((String.length !currentLoc.file) -2)) = '.'
    then (
      DoChildren
    ) else (
      
      let stores = (get_global_stores !cil_file) @ (get_local_stores f) in
      let heaps = get_local_heaps f in

      let (pre_full, pre_empty, pre_heap) = SpecParse.lookup_pre !specification f.svar.vname in
      let (post_full, post_empty, post_heap) = SpecParse.lookup_post !specification f.svar.vname in

      let (stores, heaps) = set_pre stores heaps pre_full pre_empty in

        IsEquivalent.generate_equiv f !cil_file;

        ignore (
          try 
            ignore (Apollo.apollo_func f (stores, heaps))
          with 
              E.Error -> ignore (printf "####\n# Bummer!\n####\n");
        );


        DoChildren
    )

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
  
  ("--config", Arg.String (fun config -> AddAnnotations.config_file := config),
   "Use custom configuration file for alias annotations");

  ("--spec", Arg.String (fun spec -> spec_file := spec),
   "Use specification file for pre- / post- condition specifications");

  ("--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
   "Name of the output CIL file");
  
  ("--check_headers", Arg.String (fun _ -> check_headers := true),
   "Enable checking of header files");

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
  ignore (AddAnnotations.feature.fd_doit !cil_file);

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

  let usageMsg = "Usage: rocket [options] source-file" in
  
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

    specification := (
      if (!spec_file = "") then []
      else (SpecParse.parse_spec !spec_file)
    );
      
    List.iter doFile !fileNames;
;;
    
    
(* Do stuff *)
mainFunction ();;
      



