(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

let isSysFunction fname = 
  (Str.string_match (Str.regexp "sys_") fname 0) 
;;

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

let stateVar = ref None;;
  
let sosCallType = Formatcil.cType "void * () (void *, char *, ...)" [];;
let sosCall = makeGlobalVar "SOS_CALL" sosCallType;;

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
        | Extern when not (isSysFunction v.vname) -> 
            func_extern := v::!func_extern
        | _ -> 
            (** Found a system call.  Treat these as static. *)
            func_local := v::!func_local

    );
    SkipChildren
end

(** Update code to use recently constructed SOS state structure *)
class useStateVisitor = object inherit nopCilVisitor

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

(** Change extern function calls to use SOS_CALL *)
class sosCallVisitor = object inherit nopCilVisitor

  method vinst (i: instr) =
    match i with
        Call (lop, (Lval (Var v, NoOffset)), el, loc)
          when (List.exists (fun v2 -> v.vid = v2.vid) !func_extern)
        ->
          let newInstr = 
            Formatcil.cInstr 
              "%lo:lop %l:sosCall ( %E:exps );"
              loc
              [ ("lop", Flo lop); 
                ("sosCall", Fl (Var sosCall, NoOffset)); 
                ("exps", FE ((Lval (var v))::el)) ]
          in
            ChangeTo [newInstr]
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


(** Function that produces the module header *)

let makeModHeader sub pub = 
  
  let subFuncs = 
    let count = ref (-1) in
      List.fold_left
        (fun s v ->
           count := 1 + !count;
           s ^ 
           (Printf.sprintf
              "[%d] = {XXX_%s_error_XXX, \"XXX_%s_proto_XXX\", "
              !count
              v.vname
              v.vname
           ) ^
           (Printf.sprintf
              "XXX_%s_pid_XXX, XXX_%s_fid_XXX},\n"
              v.vname
              v.vname
           )
        ) 
        ""
        sub
  in 


  let modHeader =
    Printf.sprintf
    "static const mod_header_t mod_header SOS_MODULE_HEADER = {
        .mod_id        = %s,
        .state_size    = sizeof(struct module_state),
        .num_sub_func  = %d,
        .num_prov_func = %d,
        .platform_type  = HW_TYPE,
        .processor_type = MCU_TYPE,
        .code_id       = ehtons(%s),
        .module_handler = XXX__module_handler__XXX, /* TODO: Update this name */
        .funct = {
          %s
                  },
        };
    "
      "CTOSOS_ID" 
      (List.length sub)
      (List.length pub)
      "CTOSOS_ID" 
      subFuncs    
  in
    modHeader
;;
      

let rec insertGlobal state header globals =
  match globals with
      (GFun (fd, loc))::tail -> 
        (GCompTag (state, {line=0; file="__ctosos__"; byte=0})) ::
        (GText header) ::
        (GFun (fd, loc)) ::
        tail
    | head::tail -> head :: (insertGlobal state header tail)
    | [] -> []
;;



(** Process a File *)

let doFile (file_name: string) : unit = 

  cil_file := Frontc.parse file_name ();

  (* Visit! *)
  visitCilFile (new collectGlobals) !cil_file;

  visitCilFile (new classifyFunctions) !cil_file;
 
  (* Make function pointer type *)
  let funPtrVars = 
    let fPtrType = 
      TNamed ({tname="func_cb_ptr"; 
               ttype=(TPtr ((TFun ((TVoid []), None, false, [])), [])); 
               treferenced=true}, 
              []) 
    in
      List.fold_left
        (fun fpv v ->
           (makeGlobalVar v.vname fPtrType)::fpv
        )
        []
        !func_extern
  in
           
  state := mkCompInfo
             true
             "module_state"
             (fun _ -> 
                List.rev_map 
                  (fun (vi) -> vi.vname,vi.vtype,None,[],vi.vdecl) 
                  (!global_vars @ funPtrVars)
             ) 
             []
             ;

  !cil_file.globals <- insertGlobal !state (makeModHeader !func_extern !func_global) !cil_file.globals;

  visitCilFile (new useStateVisitor) !cil_file;

  visitCilFile (new sosCallVisitor) !cil_file;


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




