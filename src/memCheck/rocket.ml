(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

(** [cil_file] CIL file being analyzed. *)
let cil_file: file ref = ref dummyFile;;


(** Helper functions *)
let get_global_stores (f: file): Apollo.store_data list = 
  foldGlobals 
    f
    (fun s g -> match g with
         GVarDecl (v, l) | GVar (v, _, l) -> 
             (Apollo.Unknown, (Lval (var v)))::s
       | GFun (fd, l) -> s
       | _ -> s
    ) 
    []
;;


let get_local_stores (f: fundec): Apollo.store_data list = 
  List.map 
    (fun v -> (Apollo.Unknown, (Lval (var v))))
    (List.filter 
       (fun v -> 
          (hasAttribute "sos_store" v.vattr) 
           || (hasAttribute "sos_claim" v.vattr)
       ) 
       (f.slocals @ f.sformals)
    )
;;
        

(** Visitor *)
class rocketVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =
    
    let stores = (get_global_stores !cil_file) @ (get_local_stores f) in
      
      IsEquivalent.generate_equiv f !cil_file;
      ignore (Apollo.apollo_func f (stores, []));

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

  (* Configuration options *)

  ("--dbg_apollo_s", Arg.Unit (fun _ -> Apollo.dbg_apollo_s := true),
   "Statement level debugging of the Apollo dataflow");

  ("--dbg_apollo_i", Arg.Unit (fun _ -> Apollo.dbg_apollo_i := true),
   "Instruction level debugging of the Apollo dataflow");

  ("--dbg_apollo_c", Arg.Unit (fun _ -> Apollo.dbg_apollo_c := true),
   "Join debugging of the Apollo dataflow");

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
      
    List.iter doFile !fileNames;
;;
    
    
(* Do stuff *)
mainFunction ();;
      



