(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

(** [cil_file] CIL file being analyzed. *)
let cil_file: file ref = ref dummyFile;;

let func_global = ref [];;
let func_local = ref [];;
let func_extern = ref [];;

(** Global vars in the original program are wrapped into a single structure in
  * the new program. *)
let global_vars = ref [];;
let state = ref 
              (mkCompInfo
                 true
                 "__ctosos_tmp_compinfo__"
                 (fun _ -> 
                    List.rev_map 
                      (fun (vi,i) -> vi.vname,vi.vtype,None,[],vi.vdecl) []) 
                 []
              )
;;

let stateVar = ref None;

(** Visitor to collect global vars *)
class collectGlobals = object inherit nopCilVisitor

  method vglob (g: global) = 
    match g with
        GVar (v, init, loc) ->
          begin match (Formatcil.dType "%t:type const" v.vtype) with
              Some _ ->
                ignore (printf "Found a const: %s\n" v.vname); 
                DoChildren
            | None -> 
                global_vars := v::!global_vars;
                let comment = 
                  sprint 
                    ~width:400 
                    (dprintf "/* %s moved to module_state structure */" v.vname)
                in
                  ChangeTo [GText comment]
          end
      | _ -> SkipChildren
end


(** Classify functions as local, static, or external *)
class classifyFunctions = object inherit nopCilVisitor

  method vvdec (v: varinfo) = 
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
    );
    SkipChildren
end

(** Visitor *)
class ctososVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =
    if not (List.exists (fun v -> v.vid = f.svar.vid) !func_local) then 
      E.s (E.bug "Interesting.  I was wondering if this could happen.");
    let stateType = TPtr ((TComp (!state, [])), []) in
      stateVar := Some (makeFormalVar f ~where:"^" "state" stateType);
      DoChildren
  
  method vlval (lv: lval) =
    let stateStruct = match !stateVar with
        None -> E.s (E.bug "stateVar must be defined at this point :-(")
      | Some v -> v
    in

    match lv with 
        (Var v, offset) when (List.exists (fun gv -> gv.vid = v.vid) !global_vars) ->
          ChangeDoChildrenPost 
            (
              (Mem (Lval (var stateStruct)), 
               Field (getCompField !state v.vname, offset)),
              (fun i -> i)
            )
      | _ -> DoChildren

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

  (* Visit! *)
  visitCilFile (new collectGlobals) !cil_file;

  state := mkCompInfo
             true
             "module_state"
             (fun _ -> 
                List.rev_map 
                  (fun (vi) -> vi.vname,vi.vtype,None,[],vi.vdecl) !global_vars) 
             []
             ;

  !cil_file.globals <- 
    (GCompTag (!state, {line=0; file="__ctosos__"; byte=0}))::!cil_file.globals;

  visitCilFile (new classifyFunctions) !cil_file;
  visitCilFile (new ctososVisitor) !cil_file;


  (* If requested dump the transformed code to file. *)
  (match !outChannel with
       None -> E.s (E.error "Must specify out file");
     | Some c -> 
         Stats.time "printCIL" 
           (dumpFile (!printerForMaincil) c !cil_file.fileName) !cil_file
  );
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




