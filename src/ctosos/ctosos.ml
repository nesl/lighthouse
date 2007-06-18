(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

let isSysFunction fname = 
  (Str.string_match (Str.regexp "sys_") fname 0) 
;;

let isKerFunction fname = 
  (Str.string_match (Str.regexp "ker_") fname 0) 
;;

let isPostFunction fname = 
  (Str.string_match (Str.regexp "post_") fname 0) 
;;


let isErrorStubFunction fname = 
  (Str.string_match (Str.regexp "error_stub_") fname 0) 
;;


let fPtrType = 
  TNamed ({tname="func_cb_ptr"; 
           ttype=(TPtr ((TFun ((TVoid []), None, false, [])), [])); 
           treferenced=true}, 
          []) 
;;

let enumCounter = ref 0;;

let typeDefProtos = ref [];;

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
let funcPtrVar = ref None;;
  
let sosCallType = Formatcil.cType "void * () (void *, char *, ...)" [];;
let sosCall = makeGlobalVar "SOS_CALL" sosCallType;;

let sysGetStateType = Formatcil.cType "void * () ()" [];;
let sysGetState = makeGlobalVar "sys_get_state" sysGetStateType;;


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
            func_global := v::!func_global;
            SkipChildren
        | Static -> 
            func_local := v::!func_local;
            SkipChildren
        | Register -> 
            E.s (E.bug "Function %s is stored in register\n" v.vname)
        | Extern when not (
            (isSysFunction v.vname) || (isKerFunction v.vname) || (isPostFunction v.vname)
          ) -> 
            func_extern := v::!func_extern;
            let (ret, formalsOp, vararg, attrs) = splitFunctionTypeVI v in
            let formals = match formalsOp with
                Some formals -> formals
              | None -> []
            in
            let ptrToSub = 
              makeGlobalVar 
                v.vname 
                (TFun 
                   (ret,  
                    Some (("proto", fPtrType, [])::formals), 
                    vararg, 
                    attrs
                   )
                ) 
            in
              typeDefProtos := ptrToSub::!typeDefProtos;
              ChangeTo ptrToSub
        | _ -> 
            (** Found a system call.  Treat these as static. *)
            func_local := v::!func_local;
            SkipChildren
    ) else (
      SkipChildren
    )
end

(** Update code to use recently constructed SOS state structure *)
class useStateVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =
    stateVar := None;
    funcPtrVar := None;

    if (List.exists (fun v -> v.vid = f.svar.vid) !func_local) then (
      (** Add module state as first parameter to each function *)
      let stateType = TPtr ((TComp (!state, [])), []) in
        stateVar := Some (makeFormalVar f ~where:"^" "state" stateType);
        DoChildren
    ) else if (List.exists (fun v -> v.vid = f.svar.vid) !func_global) then (
      SkipChildren
    ) else if (isErrorStubFunction f.svar.vname) then (
      SkipChildren
    ) else (
      E.s (E.bug "useStateVisitor: Attempt to visit non-local and non-global function %s" f.svar.vname)
    )
 
  method vinst (i: instr) =
    match i with
        Call (lop, (Lval (Var v, NoOffset)), el, loc)
          when (List.exists (fun v2 -> v.vid = v2.vid) !func_extern)
        ->
          let stateStruct = match !stateVar with
              None -> E.s (E.bug "stateVar must be defined at instruction: %a" d_instr i)
            | Some v -> v
          in

          let dummyVar = makeVarinfo true (v.vname ^ "_proto") voidType in
          let proto = Lval (var dummyVar) in
          let fnCall = (Lval (Mem (Lval (var stateStruct)), 
                              Field (getCompField !state v.vname, NoOffset))) in
          let newInstr = 
            Formatcil.cInstr 
              "%lo:lop %l:sosCall ( %E:exps );"
              loc
              [ ("lop", Flo lop); 
                ("sosCall", Fl (Var sosCall, NoOffset)); 
                ("exps", FE (fnCall::proto::el)) ]
          in
            ChangeTo [newInstr]
      | _ -> DoChildren


  method vlval (lv: lval) =
    let stateStruct = match !stateVar with
        None -> E.s (E.bug "stateVar must be defined at lval: %a" d_lval lv)
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


(** Update code to use recently constructed SOS state structure *)
class funcPtrVisitor = object inherit nopCilVisitor

  method vfunc (f: fundec) =
    funcPtrVar := None;

    if (List.exists (fun v -> v.vid = f.svar.vid) !func_local) then (
        SkipChildren
    ) else if (List.exists (fun v -> v.vid = f.svar.vid) !func_global) then (
      funcPtrVar := Some (makeFormalVar f ~where:"^" "fcb_ptr" fPtrType);
      
      let stateType = TPtr ((TComp (!state, [])), []) in
      let stateVar = makeLocalVar f "state" stateType in
      let getState = Call (Some (var stateVar), (Lval (var sysGetState)), [], f.svar.vdecl) in


      f.sbody.bstmts <- ((mkStmtOneInstr getState)::f.sbody.bstmts);
      DoChildren
    ) else if (isErrorStubFunction f.svar.vname) then (
      SkipChildren
    ) else (
      E.s (E.bug "funcPtrVisitor: Attempt to visit non-local and non-global function %s" f.svar.vname)
    )
 
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


(** Function that produces error stubs *)

let makeErrorStub (fname: string) (ftype: typ) =

  let errorStub = emptyFunction ("error_stub_" ^ fname) in

  let (returnType, _, _, _) = splitFunctionType ftype in

  let body = 
    if isVoidType returnType then (
      Formatcil.cStmt
        "return;"
        (fun n t -> E.s (E.bug "makeErrorStub: Should not be adding vars"))
        locUnknown
        []
    ) else (
      Formatcil.cStmt
        "return %retval;"
        (fun n t -> E.s (E.bug "makeErrorStub: Should not be adding vars"))
        locUnknown
        [("retval", Fd 0)]
    )
  in

    setFunctionTypeMakeFormals errorStub ftype;
    errorStub.sbody.bstmts <- [body];

    errorStub
;;


(** Function that produces the module header *)

let makeModHeader (sub: varinfo list) (pub: varinfo list) = 
  
  let count = ref (-1) in
  
  let subFuncs = 
      List.fold_left
        (fun s v ->
           count := 1 + !count;
           s ^ 
           (Printf.sprintf
              "[%d] = {error_stub_%s, \"%d\", "
              !count
              v.vname
              (Hashtbl.hash v.vtype mod 10000)
           ) ^
           (Printf.sprintf
              "sub_pid_%s, sub_fid_%s},\n"
              v.vname
              v.vname
           )
        ) 
        ""
        sub
  in 


  let pubFuncs = 
      List.fold_left
        (fun s v ->
           count := 1 + !count;
           s ^ 
           (Printf.sprintf
              "[%d] = {%s, \"%d\", "
              !count
              v.vname
              (Hashtbl.hash v.vtype mod 10000)
           ) ^
           (Printf.sprintf
              "pub_pid_%s, pub_fid_%s},\n"
              v.vname
              v.vname
           )
        ) 
        ""
        pub
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
      (subFuncs ^ pubFuncs)
  in
    modHeader
;;
      

let rec insertGlobal 
      typeDefs
      subPidEnums subFidEnums pubPidEnums pubFidEnums
      state errorStubs header globals =
  match globals with
      (GFun (fd, loc))::tail -> 
        typeDefs @ (
        (GCompTag (state, {line=0; file="__ctosos__"; byte=0})) ::
        subPidEnums :: subFidEnums :: pubPidEnums :: pubFidEnums ::
        (List.map (fun f -> GFun (f, loc)) errorStubs) @
        (
          (GText header) ::
          (GFun (fd, loc)) ::
          tail
        ))
    | head::tail -> typeDefs @ (head :: (insertGlobal 
                               []
                               subPidEnums subFidEnums pubPidEnums pubFidEnums
                               state errorStubs header tail))
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

  let errorStubs = List.map (fun v -> makeErrorStub v.vname v.vtype) !func_extern in

  let subPidEnums =
    GEnumTag (
      {
        ename="sub_pid";
        eitems=List.map 
                 (fun v -> enumCounter := !enumCounter + 1; 
                           ("sub_pid_" ^ v.vname, (integer !enumCounter), locUnknown)
                 ) !func_extern;
        eattr=[];
        ereferenced=true;
      }, 
      locUnknown)
  in

  let subFidEnums =
    GEnumTag (
      {
        ename="sub_fid";
        eitems=List.map 
                 (fun v -> enumCounter := !enumCounter + 1; 
                           ("sub_fid_" ^ v.vname, (integer !enumCounter), locUnknown)
                 ) !func_extern;
        eattr=[];
        ereferenced=true;
      }, 
      locUnknown)
  in

  let pubPidEnums =
    GEnumTag (
      {
        ename="pub_pid";
        eitems=List.map 
                 (fun v -> enumCounter := !enumCounter + 1; 
                           ("pub_pid_" ^ v.vname, (integer !enumCounter), locUnknown)
                 ) !func_global;
        eattr=[];
        ereferenced=true;
      }, 
      locUnknown)
  in

  let pubFidEnums =
    GEnumTag (
      {
        ename="pub_fid";
        eitems=List.map 
                 (fun v -> enumCounter := !enumCounter + 1; 
                           ("pub_fid_" ^ v.vname, (integer !enumCounter), locUnknown)
                 ) !func_global;
        eattr=[];
        ereferenced=true;
      }, 
      locUnknown)
  in


  let typeDefs = 
    List.map
      (fun v -> 
         GType (
           {
             tname=(v.vname ^ "_proto");
             ttype=v.vtype;
             treferenced=false
           }, 
           locUnknown))
      !typeDefProtos
  in


  !cil_file.globals <- insertGlobal 
                         typeDefs
                         subPidEnums subFidEnums pubPidEnums pubFidEnums
                         !state 
                         errorStubs 
                         (makeModHeader !func_extern !func_global) 
                         !cil_file.globals;

  visitCilFile (new useStateVisitor) !cil_file;
  visitCilFile (new funcPtrVisitor) !cil_file;
  
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




