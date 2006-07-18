open Cil

module E = Errormsg

(* Helper function to open a file for output *)
let openFile (takeit: out_channel -> unit) outFile = 
  if !E.verboseFlag then
    ignore (Printf.printf "Setting output to %s\n" outFile);
  (try takeit (open_out outFile)
   with _ ->
     raise (Arg.Bad ("Cannot open output file " ^ outFile)))
;;


(* Output channel that will be used by CIL *)
let outChannel : out_channel option ref = ref None;;


(* Command line options that will be recognizied by this program  *)
let argDescr = [
      ("--out", Arg.String (openFile (fun oc -> outChannel := Some oc)),
             "the name of the output CIL file");
      (* ADD OTHER OPTIONS HERE *)
];;
  

(* Run a given file thtrough CIL *)
let doFile fn = 
   
  let f = Frontc.parse fn () in  

    (* ADD CALLS TO OTHER PARTS OF CIL HERE using a style similar to: *)
    (*     ignore (MyModule.feature.fd_doit f); *)

    (match !outChannel with
         None -> ()
       | Some c -> Stats.time "printCIL" 
                     (dumpFile (!printerForMaincil) c f.fileName) f);
;;


(* Handle the command line, initialize CIL, and then process the incoming file.*)
let mainFunction () =

  let fileNames : string list ref = ref [] in

  let commandLine () = 
    let usageMsg = "Usage: " ^ Sys.argv.(0) ^ " [options] source-file" in
    let recordFile fname = fileNames := fname :: (!fileNames) in
    let outName = ref "" in
      Arg.parse argDescr recordFile usageMsg;
      fileNames := List.rev !fileNames;
      if not (List.length !fileNames == 1) then (
        Arg.usage argDescr usageMsg;
        exit 0;
      )
  in

  let runCil () =
    Cil.initCIL ();
    List.iter doFile !fileNames;
  in

    (* Go! *)
    commandLine ();
    runCil ();
    ()
;;


mainFunction ();;

