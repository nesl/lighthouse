(** Helper functions for scripting tools to prepare a program in CIL
  @author Roy Shea (roy\@cs.ucla.edu)
  *)

open Cil
module E = Errormsg

(* Output channel that will be used by CIL if the user requests to save modified
 * program. *)
let outChannel : out_channel option ref = ref None;;


(* Helper function to open a file for output *)
let openFile (takeit: out_channel -> unit) outFile = 
  if !E.verboseFlag then
    ignore (Printf.printf "Setting output to %s\n" outFile);
  (try takeit (open_out outFile)
   with _ ->
     raise (Arg.Bad ("Cannot open output file " ^ outFile)))
;;


(* Command line options that will be recognizied by this program  *)
let argDescr = [
      ("--out", Arg.String (openFile (fun oc -> outChannel := Some oc)),
             "the name of the output CIL file");
      (* ADD OTHER OPTIONS HERE *)
];;
  

(* Run a given file through CIL *)
(* ADD CALLS TO OTHER PARTS OF CIL HERE using a style similar to: *)
(*     ignore (MyModule.feature.fd_doit f); *)
let makeCilFile fileName = 
  Cil.initCIL ();
  Frontc.parse fileName ()
;;


(* Handle the command line, initialize CIL, and then process the incoming file.*)
let mainFunction () =

  (* Input and output file names *)
  let inName = ref "" in
  let outName = ref "" in

  (* Parse command line for the name of input file and optional output file name *)
  let parseCommandLine () = 
    let usageMsg = "Usage: " ^ Sys.argv.(0) ^ " [options] source-file" in
    let getInputFile name = 
      if !inName = "" then (
        inName := name 
      ) else (
        E.s (E.error "mainFunction: May only specify one source-file on the command line")
      )
    in
      Arg.parse argDescr getInputFile usageMsg;
      if !inName = "" then 
        E.s (E.error "mainFunction: Must specify one source-file on the command line");
      ()
  in

    (* Go! *)
    parseCommandLine ();
    let cilFile = makeCilFile !inName in
      (match !outChannel with
           None -> ()
         | Some c -> Stats.time "printCIL" 
                       (dumpFile (!printerForMaincil) c cilFile.fileName) cilFile);
      ()
;;

(* mainFunction ();; *)

