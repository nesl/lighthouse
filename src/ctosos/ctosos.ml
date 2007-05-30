(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

(** [cil_file] CIL file being analyzed. *)
let cil_file: file ref = ref dummyFile;;

let global_vars = ref [];;

let func_global = ref [];;
let func_local = ref [];;
let func_extern = ref [];;

(** Visitor *)
class ctososVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =

    ignore (Pretty.printf "Found function %s\n" f.svar.vname);
    DoChildren

  method vvdec (v: varinfo) = 

    ignore (Pretty.printf "   -> variable %s: %a" v.vname d_storage v.vstorage);
    if v.vglob then (
      if isFunctionType v.vtype then (
        match v.vstorage with
            NoStorage -> 
              func_global := v::!func_global
          | Static -> 
              func_local := v::!func_local
          | Register -> 
              E.s (E.bug "Function %s is stored in register\n" v.vname)
          | Extern -> 
              func_extern := v::!func_extern
      ) else (
        global_vars := v::!global_vars
      );
      Pretty.printf " global\n")
    else (Pretty.printf "\n");


    DoChildren

end


(** Utility function used to open a file for writing the transformed program. *)
let openFile (what: string) (takeit: out_channel -> unit) (fl: string) = 
  (try takeit (open_out fl)
   with _ ->
     raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))
;;


(** Reference to the channel that will be used to output the transformed code to
  * file. *)
let outChannel : out_channel option ref = ref None;;


(** Descrimption of the command line interface to Lighthouse. *)
let argDescr = [

  ("--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
   "Name of the output CIL file");

];;


(** Process a File *)

let doFile (file_name: string) : unit = 

  cil_file := Frontc.parse file_name ();

  (* If requested dump the transformed code to file. *)
  (match !outChannel with
       None -> E.s (E.error "Must specify out file");
     | Some c -> 
         Stats.time "printCIL" 
           (dumpFile (!printerForMaincil) c !cil_file.fileName) !cil_file
  );

  (* Visit! *)
  visitCilFileSameGlobals (new ctososVisitor) !cil_file;

  ()
;;


(** Read and process the command line, and then run Lighthouse on the requested
  * file. *)
let mainFunction () =

  let usageMsg = "Usage: ctosos --out <outfile> source-file" in

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

    List.iter (fun v -> ignore (Pretty.printf "---> %s\n" v.vname)) !func_global;
    List.iter (fun v -> ignore (Pretty.printf "~~~> %s\n" v.vname)) !func_local;
    List.iter (fun v -> ignore (Pretty.printf "^^^> %s\n" v.vname)) !func_extern;
    List.iter (fun v -> ignore (Pretty.printf "***> %s\n" v.vname)) !global_vars;
;;


(* Do stuff *)
mainFunction ();;




