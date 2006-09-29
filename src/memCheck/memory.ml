(** Driver module for the Lighthouse application *)

(* Core CIL functionality *)
open Pretty;;
open Cil;;
module E = Errormsg;;

(* Custom analysis *)
module ID = IsDead;;
module IS = IsStored;;
module CA = CallerAllocates;;
module MU = MemUtil;;
module IE = IsEquivalent;;
module MA = MayAliasWrapper

(** {1 Overview of Transformations} 
  *
  * Lighthouse performs a number of transformations to produce a C file
  * that is easier to analyze.  These transformations include:
  * 
  * - [Simplemem] all lvalues involve at most one memory reference
  * - [Simplify] a form of three-address code
  * - [Oneret] all function bodies have at most one return statement
  * - [MakeOneCFG] seperate each instruction into its own [Cil.stmt] 
  * - [Ptranal] interprocedural points-to analyses (must be run exactly once)
  * - [AddAnnotations] use external file to add attributes to function formals
  *)                                      
 


(** {1 Debugging Options}
  *
  * Enable more verbose output from the analysis.  Usefull for debugging
  * purposes.
  *)
let dbg_free_exp = ref false;;
let dbg_alloc_exp = ref false;;
let dbg_alloc_stores = ref false;;
let dbg_ptr_arith = ref false;;



(** {1 Global State}
  *
  * Lighthouse uses global state to shuttle information between different parts
  * of its analysis.  These states include:
  *)

(** [global_stores] Global variables. *)
let global_stores: exp list ref = ref [];;

(** [local_stores] Local variables that have the "sos_store" attribute set
  * indicating that they may be used to persistently store data. *)
let local_stores: exp list ref = ref [];;

(** [cil_file] CIL file being analyzed. *)
let cil_file: file ref = ref dummyFile;;

(** {2 State Utility Functions}
  *
  * Along with the global state are utility functions used to help gather the
  * state.  At some point the global variables may be replaced with direct
  * function calls to generate the state of interest. 
  *)

let get_global_vars (f: file): exp list = 
  foldGlobals 
    f
    (fun s g -> match g with
         GVarDecl (v, l) | GVar (v, _, l) -> (Lval (var v))::s
       | GFun (fd, l) -> s
       | _ -> s
    ) 
    []
;;


let get_local_stores (f: fundec): exp list = 
  List.map 
    (fun v -> (Lval (var v)))
    (List.filter 
       (fun v -> 
          (hasAttribute "sos_store" v.vattr) 
           || (hasAttribute "sos_claim" v.vattr)
       ) 
       (f.slocals @ f.sformals))
;;
        

(** {2 White List} 
  *
  * Simple mechanism to white list a few "bottem" functions.
  *)
let white_list (v:varinfo) : bool =
  let skip_functions = ["ker_malloc"; "ker_free"] in
    List.exists (fun skip -> v.vname = skip) skip_functions
;;
    


(** {1 Lighthouse Visitor} 
  *
  * This is where all the action is.  This visitor traverses a [Cil.file] to
  * look for violations of a strict "exactly once" resource ownership model.
  * The basic idea behind this model is that a given resource is always owned by
  * exactly one entity.  Ownership can be transfered, but never deplicated or
  * removed.
  *
  * An example of this can be seen in memory allocation.  The [malloc] operation
  * in C can be thought of as transfering ownership for a block of memory from
  * the kernel to a function.  Conversly, [free] transfers memory from the
  * function back to the kernel.  A memory leak appears as a failure for the
  * function to properly (presestantly) store a reference to the memory it
  * allocated.  Attempting to dereferece a pointer to freed memory appears as a
  * violation of each resource having exactly one owner, since the freed memory
  * is now owned by the kernel but a user function is trying to access it.
  *)

class memoryVisitor = object inherit nopCilVisitor

  (** {2 Visitor State} *)
                               
  (** Global state used to leak statement information to other parts of the
    * visitor. *)
  val currentStmt = ref (mkEmptyStmt ());
  val currentFunc = ref dummyFunDec;

                    
  (** {2 Visitor Methods} *)

  (** {3 Function Visitor} *)
  method vfunc (f:fundec) = 

    (** The function visitor looks for violations of the promises made by the
      * function prototype.  The visitor checkes that:
      *
      * - formals with the "sos_release" attribute are either stored or released
      * by the function
      * 
      * - formals (or return value) with the "sos_claim" attribute set referece
      * dynamicly memory that can be released to the caller by the end of the
      * function
      *
      * This is accomplished via the following steps *)

    (** - Note the current function *)
    currentFunc := f;

    (** - Update the must alias analysis for this function *)
    IE.generate_equiv f !cil_file;

    (* - Generate the local stores for this function *)
    local_stores := get_local_stores f;

    (** - Ensure that the function properly stores any data that it allocates.
      *)
    let bad_storage = 
      IS.not_stored_exps f (!local_stores @ !global_stores)
    in

    (** - Ensure that each formal with the "sos_claim" attribute set is allocated
      * by the function, so that it can be claimed by the caller. *)
    let bad_formal_allocation = 
      List.filter  
        (fun v -> not (CA.var_is_allocated v f))
        (List.filter (fun v -> hasAttribute "sos_claim" v.vattr) f.sformals)
    in

    (** - If the return value has the "sos_claim" attribute set, ensure that this
      * function is allocating data to return to the caller. *)
    let (return_type, _, _, _) = splitFunctionType f.svar.vtype in
    let bad_return_allocation = 
      if (hasAttribute "sos_claim" (typeAttrs return_type)) then 
        not (CA.return_is_allocated f)
      else 
        false
    in


      (** - Print error messages for any errors identified from the above checkes. *)

      List.iter 
        (fun e -> 
           if not (white_list f.svar) then
             E.error "Function %s fails to store expression %a" f.svar.vname d_exp e) 
        bad_storage;

      List.iter 
        (fun v -> 
           if not (white_list f.svar) then
             E.error "Function %s fails to fill formal variable %s" f.svar.vname v.vname) 
        bad_formal_allocation;

      if (bad_return_allocation && not (white_list f.svar)) then
        E.error "Function %s fails to return allocated memory" f.svar.vname;

      DoChildren


        
  (** {3 Statment Visitor} *)
  method vstmt (s:stmt) =

    (** The statement visitor simply updates a global reference to track the
      * current statement.  This information is importent to analysis done by
      * the underlying instruction level visitor. *)
    currentStmt := s;
    DoChildren

      
      
  (** {3 Instruction Visitor} *)
  method vinst (i:instr) =
 
    (** Identify all expressions that this instruction improperly releases.
    * This is accomplished by: *)
    
    (** - Ensure that any expression that is released by this instruction is
      * treated as dead until the end of the current function. *)
    let bad_releases = 
      List.filter
        (fun e -> not (ID.is_dead e !currentStmt))
        (MU.get_released i)
    in

      (** - Alert the user to any violations. *)

      List.iter
        (fun e -> E.error "Expression %a is not treated as dead after instruction %a"
                    d_exp e d_instr i)
        bad_releases;

      DoChildren
      
end


(** {1 Utility and Driver Functions} *)

(** {2 File Output} *)

(** Utility function used to open a file for writing the transformed program
  * that Lighthouse then analyzes.  This transformed program should be
  * functionally equivalent to the original. *)
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

  ("--enable_loop", Arg.Unit (fun _ -> ID.enable_loop := true),
   "Enable listing of data freed in loops");

  ("--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
   "Do not remove the unused variables and types");

  ("--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
   "Name of the output CIL file");

  (* Debugging for IsDead *)

  ("--mem_dbg_free_exp", Arg.Unit (fun _ -> dbg_free_exp := true),
   "Print freed expressions");

  ("--dbg_is_dead_i", Arg.Unit (fun _ -> ID.dbg_is_dead_i := true),
   "Instruction level debugging of the IsDead dataflow");

  ("--dbg_is_dead_s", Arg.Unit (fun _ -> ID.dbg_is_dead_s := true),
   "Statement level debugging of the IsDead dataflow");

  ("--dbg_is_dead_c", Arg.Unit (fun _ -> ID.dbg_is_dead_c := true),
   "Join debugging of the IsDead dataflow");

  (* Debugging for IsStore *)

  ("--mem_dbg_alloc_exp", Arg.Unit (fun _ -> dbg_alloc_exp := true),
   "Print alloced expressions");

  ("--dbg_is_store", Arg.Unit (fun _ -> IS.dbg_is_store := true),
   "Enable debugging of the IsStore analysis");

  (* Debugging for alias analysis MayAliasWrapper and IsEquivalent *)

  ("--dbg_may_alias", Arg.Unit (fun _ -> MA.dbg_may_alias := true),
   "Enable more verbose output from the may alias analysis");

  ("--dbg_is_equiv_i", Arg.Unit (fun _ -> IE.dbg_is_equiv_i := true),
   "Instruction level debugging of the IsEquivalent dataflow");

  ("--dbg_is_equiv_c", Arg.Unit (fun _ -> IE.dbg_is_equiv_c := true),
   "Join debugging of the IsEquivalent dataflow");

  ("--dbg_is_equiv_stmt_summary", Arg.Unit (fun _ -> IE.dbg_is_equiv_stmt_summary := true),
   "Dump summary of incoming statement equivalency sets generated by IsEquivalent dataflow");

  ("--dbg_is_equiv_get_aliases", Arg.Unit (fun _ -> IE.dbg_is_equiv_get_aliases := true),
   "Dump the result of calls to get_aliases within the IsEquivalent dataflow");
  
  ("--dbg_is_equiv_get_equiv_set", Arg.Unit (fun _ -> IE.dbg_is_equiv_get_equiv_set := true),
   "Dump the result of calles to get_equiv_set within the Isequivalent dataflow");

  (* Degbugging for CallerAllocates *)

  ("--dbg_caller_allocate_i", Arg.Unit (fun _ -> CA.dbg_caller_allocates_i := true),
   "Insturtion level debugging of the CallerAlocates dataflow");

  ("--dbg_caller_allocate_s", Arg.Unit (fun _ -> CA.dbg_caller_allocates_s := true),
   "Statement level debugging of the CallerAlocates dataflow");

  ("--dbg_caller_allocate_c", Arg.Unit (fun _ -> CA.dbg_caller_allocates_c := true),
   "Join debugging of the CallerAlocates dataflow");

];;


(** {2 Process a File} *)

(** This is the core driver for Lighthouse.  It explicitly calls the
  * transformations required for Lighthouse to analyzie the file, and then
  * visits the transformed file looking for violations of the basic resource
  * model. *)
let doFile (file_name: string) : unit = 

  cil_file := Frontc.parse file_name ();

  (* Execute other modules in the correct order *) 
  ignore (Simplemem.feature.fd_doit !cil_file);
  ignore (Simplify.feature.fd_doit !cil_file);
  ignore (Oneret.feature.fd_doit !cil_file);
  ignore (MakeOneCFG.make_one_cfg !cil_file);
  ignore (Ptranal.feature.fd_doit !cil_file);
  ignore (AddAnnotations.feature.fd_doit !cil_file);

  (* Generate a table to note persistent stores *)
  global_stores := get_global_vars !cil_file;

  (* If requested dump the transformed code to file. *)
  (match !outChannel with
       None -> ()
     | Some c -> Stats.time "printCIL" 
                   (dumpFile (!printerForMaincil) c !cil_file.fileName) !cil_file);

  (* Visit! *)
  visitCilFileSameGlobals (new memoryVisitor) !cil_file;

  ()
;;


(** {2 Read Command Line} *)

(** Read and process the command line, and then run Lighthouse on the requested
  * file. *)
let mainFunction () =

  let usageMsg = "Usage: memory [options] source-file" in
  
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
      


