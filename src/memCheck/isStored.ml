open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module IE = IsEquivalent
module E = Errormsg
 

(************************************************************)
(* General configuration of the dataflow for a specific run *)
(************************************************************)

(* Blah... *)             
let current_stmt_id = ref (-1);;
let claim_on_return = ref false;;

(* Reference to the varinfo ID that we are interested in *)
let target: exp ref = ref mone;;

(* Reference to valid locations to store target *)
let stores: exp list ref = ref [];;
                
(* Types to describe:
 * - Data flow states
 * - Effects of instructions on data flow
 *)
type status = MustTake | Taken | Error | Null;;
type instr_status = IStore | INoEffect | IOverWrite;;

(* Runtime debugging flags. *)
let dbg_is_store_i = ref false;;
let dbg_is_store_s = ref false;;
let dbg_is_store_g = ref false;;
let dbg_is_store_c = ref false;;
 
(* Dataflow specific debugging. *)
let dbg_is_store_df = ref false;;



(************************************************************)
(* Helper functions *)
(************************************************************)

(* An instruction can:
 * - use a Set or Call to overwrite the target expression
 * - use a Set to save the target into a store
 * - use a Call to release the target
 * - have no effect on the target
 *)
let instr_stores (i: instr) (sid: int) : instr_status = 

  let is_store (e: exp) (id: int) : bool =
    List.exists (fun store -> IE.is_equiv e store sid) !stores
  in

  let is_target (e: exp) (id: int) : bool =
    IE.is_equiv e !target sid 
  in
  
  let is_released (i: instr) (id: int) : bool = 
    List.exists
      (fun release -> IE.is_equiv release !target id)
      (U.get_released i)
  in
    
  let is_overwritten (i: instr) (id: int) : bool = 
    List.exists
      (fun release -> IE.is_equiv release !target id)
      (U.get_claim i)
  in

    if !dbg_is_store_i then (
      ignore (printf "IsStored.instr_stores trying to store \"%a\":\n" d_exp !target);
      ignore (printf "...is_released: %b\n" (is_released i sid));
      ignore (printf "...is_overwritten: %b\n" (is_overwritten i sid));
      (match i with
           Set (lv, e, _) ->
             ignore (printf "...is_target \"%a\": %b\n" d_lval lv (is_target (Lval lv) sid));
             ignore (printf "...is_store \"%a\": %b\n" d_lval lv (is_store (Lval lv) sid));
             ignore (printf "...is_target \"%a\": %b\n" d_exp e (is_target e sid));
             ignore (printf "...is_store \"%a\": %b\n" d_exp e (is_store e sid));
         | Call (Some lv, _, _, _) ->
             ignore (printf "...is_target \"%a\": %b\n" d_lval lv (is_target (Lval lv) sid));
         | _ -> ()
      );
      flush stdout;
    );
    
    match i with
      | Set (lv, _, _) when (is_target (Lval lv) sid)
        -> IOverWrite
      | Call (Some lv, _, _, _) when (is_target (Lval lv) sid)
        -> IOverWrite
      | Call (_, _, _, _) when (is_overwritten i sid)
        -> IOverWrite
      
      | Set (lv, e, _) when (
          (is_store (Lval lv) sid) && 
          (is_target e sid)
        ) -> IStore

      | Call (_, _, _, _) when (is_released i sid) 
        -> IStore

      | _ -> INoEffect
;;
          


(************************************************************)
(* Dataflow module to check if data is properly stored or released *)
(************************************************************)

module DFO = struct

  (* Vital stats for this dataflow. *)
  let name = "ownFlow";;
  let debug = dbg_is_store_df;;
  type t = status;;

 
  (* Basic util functions to jumpstart dataflow. *)           
  let stmtStartData: t IH.t = IH.create 17
  let copy (state: t) = state
  let pretty () (state: t) =
    dprintf "{%s}" ( 
      match state with 
          MustTake -> "MustTake"
        | Taken -> "Taken"
        | Null -> "Null"
        | Error -> "Error"
    )
  ;;


  (****************************************)
  (* Helper functions used within the dataflow for debugging *)  
  (****************************************)
  
  let debug_combine (s: stmt) (transition: t option) (old_state: t): unit =
    if !dbg_is_store_c then (
      ignore (printf "IsStored.DFO.combinePredecessors: ");
      ignore (printf "Join at statement %a" d_stmt s);
      match transition with
          Some new_state -> ignore (printf "Transitions from %a to %a\n" 
                                  pretty old_state pretty new_state);
        | None -> ignore (printf "Stays in %a\n" pretty old_state);
    );
    flush stdout;
  ;;


  let debug_instr (i: instr) (transition: t DF.action) (old: t): unit =
    if !dbg_is_store_i then (
      ignore (printf "IsStored.DFO.doInstr: Instruction  %a\nwith effect:\n" d_instr i);
      match transition with
          DF.Done new_state -> ignore (printf "Transitions from %a to %a\n" 
                                         pretty old pretty new_state);
        | DF.Default -> ignore (printf "Stays in %a\n" pretty old);
        | _ -> E.s (E.bug "unexpected transition from instruction\n")
    );
    flush stdout;
  ;;

  
  let debug_guard (e: exp) (transition: t DF.guardaction) : unit =
    if !dbg_is_store_g then (
      ignore (printf "IsStored.DFO.doGuard: Expression %a" d_exp e);
      match transition with
          DF.GUse new_state -> ignore (printf " propogates %a\n" pretty new_state);
        | DF.GDefault -> ignore (printf " has no special effect\n");
        | _ -> E.s (E.bug "unexpected evaluation of guard\n")
    );
    flush stdout;
  ;;

    
  let debug_stmt (s: stmt) (transition: t DF.stmtaction) : unit =
    if (!dbg_is_store_s) then (
      ignore (printf "IsStored.DFO.doStmt: Examining statement %d \n%a\nwith effect:" 
                s.sid d_stmt s);
      match transition with 
          DF.SUse new_state -> ignore (printf " propogates %a\n" pretty new_state);
        | DF.SDefault -> ignore (printf " has no special effect\n");
        | _ -> E.s (E.bug "unexpected evaluation of guard\n")
    );
    flush stdout;
  ;;


  (****************************************)
  (* Core dataflow functions *)  
  (****************************************)

  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: status): status = state;;

  (* Dataflow types are releated via:
   * - Error dominating all
   * - Taken dominating Null
   * - MustTake dominating Null
   * - Everything else causes an error
   *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) = 

    let transition = match (new_state, old) with
      | (Null, Null) 
      | (Taken, Taken)
      | (MustTake, MustTake)
      | (_, Error) -> None
      
      | (MustTake, Null) -> Some MustTake
      | (Null, MustTake) -> None
      
      | (Taken, Null) -> Some Taken
      | (Null, Taken) -> None

      | _ -> Some Error
    in

      debug_combine s transition old;
      transition
  ;;


  (* Examine how an instruction effects the target. *)
  let doInstr (i: instr) (state: t): t DF.action = 
    
    let transition = match (state, (instr_stores i !current_stmt_id)) with

        (Error, _)
      | (_, INoEffect) ->
          DF.Default
      
      | (Taken, IOverWrite)
      | (Null, IOverWrite) ->
          (* Okay to overwrite a target knowen to be Null or stored *)
          DF.Default
            
      | (MustTake, IOverWrite) -> 
          (* Overwriting target before storing it causes a memory leak *)
          DF.Done Error

      | (Taken, IStore) ->
          (* Double store is bad *)
          DF.Done Error

      | (MustTake, IStore)
      | (Null, IStore) -> 
          DF.Done Taken
      
    in

      debug_instr i transition state;
      transition
  ;;


  (* Statement can "store" data by returning the target data in a return clause
   * that has the sos_claim attribute set. *)
  let doStmt (s: stmt) (state: t) = 
    current_stmt_id := s.sid;

    let transition = match s.skind with 
        Return (Some e, _) when (
          (IE.is_equiv e !target s.sid) && !claim_on_return
        ) ->

          if (state = MustTake || state = Null) then (
            IH.replace stmtStartData s.sid Taken;
            DF.SDone
          ) else (
            IH.replace stmtStartData s.sid Error;
            DF.SDone
          )
        
      | _ -> 
          DF.SDefault
    in

      debug_stmt s transition;
      transition
  ;;

  
  (* Special cases for when an if statment is checking to see if the target
   * expression is Null.  An example of this is checking to see if a call to
   * malloc fails.  When the expression is Null, it can be considered to have
   * been claimed. *)
  let doGuard (e: exp) (state: t) = 
    
    (* Helper function used to determine if a binary comparison is comparing the
     * target expression to a null pointer *)
    let is_target_and_null (e1: exp) (e2: exp) (id: int) : bool =
      let is_target = 
        (IE.is_equiv e1 !target id) || (IE.is_equiv e2 !target id) 
      in
      let is_null = 
        (IE.is_equiv e1 IE.nullPtr id) || (IE.is_equiv e2 IE.nullPtr id)
      in
        is_target && is_null
    in
      
    let transition = match e with

      | Lval lv 
          when (IE.is_equiv (Lval lv) !target !current_stmt_id) ->
          (* Unary check to see if item is NOT null.  Continue on with the
           * default action to ensure that the target is stored. *)
          DF.GDefault

      | UnOp (LNot, (Lval lv), _) 
          when (IE.is_equiv (Lval lv) !target !current_stmt_id) ->
          (* Unary check to see if item is Null.  Since we know that the target is
           * Null we can abort the check for this branch and directly insert the Null
           * state. *)
          DF.GUse Null

      | UnOp (LNot, (UnOp (LNot, (Lval lv), _)), _)
          when (IE.is_equiv (Lval lv) !target !current_stmt_id) ->
          (* Unary check to see if item is NOT Null.  This form results from the
           * elseGuard clause within the dataflow engine. Continue with default
           * action. *)
          DF.GDefault

      | BinOp (Ne, e1, e2, _) 
          when (is_target_and_null e1 e2 !current_stmt_id) ->
          (* Binary check to see if item is NOT null *)
          DF.GDefault

      | BinOp (Eq, e1, e2, _)
          when (is_target_and_null e1 e2 !current_stmt_id) ->
          (* Binary check to see if item is Null *)
          DF.GUse Null
         
      | UnOp (LNot, (BinOp (Ne, e1, e2, _)), _)
          when (is_target_and_null e1 e2 !current_stmt_id) ->
          (* Binary check to see if item is NOT NOT null.  This form results
           * from the elseGuard clause within the dataflow engine. *)
          DF.GUse Null

      | UnOp (LNot, (BinOp (Eq, e1, e2, _)), _)
          when (is_target_and_null e1 e2 !current_stmt_id) ->
          (* Binary check to see if item is NOT Null.  This form results 
           * from the elseGuard clause within the dataflow engine. *)
          DF.GDefault

      | _ -> 
          DF.GDefault
    in

      debug_guard e transition;
      transition
  ;;
  
  (* All blocks go on worklist. *)                                                    
  let filterStmt _ = true;;

end



(************************************************************)
(* Instantiate the dataflow module *)
(************************************************************)

module Track = DF.ForwardsDataFlow(DFO);;



(************************************************************)
(* Interface functions to the dataflow *)
(************************************************************)

(* Lower layer interface is not currently used *)
(*
let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)
;;
 *)

let get_return_statements (f: fundec) : stmt list =
  List.filter 
    (fun s -> match s.skind with Return _ -> true | _ -> false) 
    f.sbody.bstmts
;;


let set_claim_on_return (f: fundec) : unit =
  claim_on_return := match f.svar.vtype with
      TFun (t, _, _, _) when (hasAttribute "sos_claim" (typeAttrs t)) -> true
    | _ -> false
;;


let is_stored (f: fundec) : bool =
  List.for_all 
    (fun s -> 
       try match (IH.find DFO.stmtStartData s.sid) with
           Taken | Null -> true
         | MustTake | Error -> false
       with Not_found -> false)
    (get_return_statements f)
;;


(* Check to see if the formal variable v is stored within function f *)
let is_stored_func (e: exp) (f: fundec) (new_stores: exp list) : bool = 
  
  target := e;
  set_claim_on_return f;
  stores := new_stores;
             
  IH.clear DFO.stmtStartData;
  IH.add DFO.stmtStartData (List.hd f.sbody.bstmts).sid MustTake;
  Track.compute [List.hd f.sbody.bstmts];
 
  is_stored f  
;;


(* Check to see that the expression e is stored before the end of the current
 * function f *)
let is_stored_instr
      (e: exp) (s: stmt) (i: instr) (f: fundec) (new_stores: exp list)
      : bool = 
  
  target := e;
  set_claim_on_return f;
  stores := new_stores;
             
  IH.clear DFO.stmtStartData;

  begin match i with
      Set (lv, _, _) 
    | Call (Some lv, _, _, _) when (
        List.exists (fun store -> IE.is_equiv (Lval lv) store s.sid) !stores
      ) -> IH.add DFO.stmtStartData s.sid Taken;
    | _ -> IH.add DFO.stmtStartData s.sid MustTake;
  end;

  Track.compute [s];
     
  is_stored f  
;;
  

