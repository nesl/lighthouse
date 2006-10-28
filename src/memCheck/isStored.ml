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
type status = MustTake | Taken | Error | Null | ReturnTaken;;
type instr_status = IStore | INoEffect | IOverWrite;;

(* Runtime debugging flags. *)
let dbg_is_store_i = ref false;;
let dbg_is_store_s = ref false;;
let dbg_is_store_g = ref false;;
let dbg_is_store_c = ref false;;
let verbose = ref false;;

(* Dataflow specific debugging. *)
let dbg_is_store_df = ref false;;


(************************************************************)
(* Helper functions *)
(************************************************************)


(* A valid form of storing an expression e is to store it:
 * 
 * - directly into a store s such as:
 *     s = e
 * - into a field of a store s such as either of:
 *     s.a = e
 *     s->b = e
 * 
 * This function is used to see if an expression is either one of a set of
 * targets OR a member of a field of a target.
 *)
let is_field_of (e: exp) (targets: exp list) (sid: int) : bool =

  let rec is_field_of_helper (el: exp list) =

    let direct = 
      List.exists 
        (fun target -> 
           List.exists (fun e -> IE.is_equiv e target sid) el
        )
        targets 
    in

      if !verbose then (
        ignore (printf "Seeing if any of (via %a) in statement %d:\n" d_exp e sid);
        List.iter (fun e -> ignore (printf "    %a\n" d_exp e)) el;
        ignore (printf " are equivalent to a field of:\n");
        List.iter (fun e -> ignore (printf "    %a\n" d_exp e)) targets;
        ignore (printf "with result %b\n" direct)
      );

      if direct then true 
      else (
        List.exists
          (fun e ->
             match (stripCasts e) with 
                 Lval (Var v, Field _) 
               | Lval (Var v, Index _) ->
                   is_field_of_helper (IE.get_equiv_set (Lval (var v)) sid)

               | Lval (Mem e, _) -> 
                   is_field_of_helper (IE.get_equiv_set e sid)

               | BinOp (IndexPI, e, _, _)
               | BinOp (MinusPI, e, _, _) ->
                   is_field_of_helper (IE.get_equiv_set e sid)

               | BinOp (PlusA, e1, e2, _)
               | BinOp (MinusA, e1, e2, _) ->
                   is_field_of_helper (IE.get_equiv_set e1 sid) || 
                   is_field_of_helper (IE.get_equiv_set e2 sid)

               | _ -> 
                   false
          )
          el
      )
  in

    is_field_of_helper (IE.get_equiv_set e sid)
;;


(* An instruction can:
 * 
 * - use a Set or Call to overwrite the target expression
 * - use a Set to save the target into a store
 * - use a Call to release the target
 * - have no effect on the target
 * 
 * There is another posiblity.  A Call instruction could directly save allocated
 * memory into a store. Ie:
 *     store = malloc(size);
 * This is a special case that can only occure in the instruction that causes
 * the allocation.  As such, we handle it elsewhere and leave this as a more
 * specific function for analyizing instruction AFTER the allocation point.
 *)
let instr_stores (i: instr) (sid: int) : instr_status = 

  
  let is_released (i: instr) (id: int) : bool = 
    let released_formals = U.get_released i in
      (List.length released_formals > 0) &&
      (List.exists (fun release -> IE.is_equiv release !target id) released_formals)
  in
  
  
  let is_overwritten (i: instr) (id: int) : bool = 
    let claimed_formals = U.get_claim i in
    (List.length (claimed_formals) > 0) &&
    (List.exists (fun release -> IE.is_equiv release !target id) claimed_formals
    )
  in
    
  
  let action = match i with
    (* TODO: These three could be incorrect if the target is also passed as a
     * formal in the expression *)
    | Set (lv, _, _) when (IE.is_equiv (Lval lv) !target sid) -> IOverWrite
    | Call (Some lv, _, _, _) when (IE.is_equiv (Lval lv) !target sid) -> IOverWrite
    | Call (_, _, _, _) when (is_overwritten i sid) -> IOverWrite

    | Set (lv, e, _) when ( 
        (is_field_of (Lval lv) !stores sid) && 
        (is_field_of !target [e] sid) 
      ) -> IStore

    | Call (_, _, _, _) when (is_released i sid) -> IStore

    | _ -> INoEffect
  in

      action
;;
          


(************************************************************)
(* Dataflow module to check if data is properly stored or released *)
(************************************************************)

module DFO = struct

  (* Vital stats for this dataflow. *)
  let name = "is_stared_flow";;
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
        | ReturnTaken -> "ReturnTaken"
    )
  ;;


  (****************************************)
  (* Helper functions used within the dataflow for debugging *)  
  (****************************************)
 
  let debug_combine_head (s: stmt) (new_state: t) (old_state: t): unit =
    if !dbg_is_store_c then (
      ignore (printf "IsStored.DFO.combinePredecessors: Entering statement %d (%a) with states:\n"
                s.sid d_loc (get_stmtLoc s.skind)
      );
      ignore (printf "old: %a, new: %a\n" pretty old_state pretty new_state);
      flush stdout;
    );
  ;;
  
  
  let debug_combine_tail (s: stmt) (transition: t option) (old_state: t): unit =
    if !dbg_is_store_c then (
      ignore (printf "IsStored.DFO.combinePredecessors: ");
      ignore (printf "Join at statement (%a):\n%a\n" d_loc (get_stmtLoc s.skind) d_stmt s);
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
      ignore (printf "IsStored.DFO.doGuard: Expression %a guarding statement %d\n" 
                d_exp e !current_stmt_id);
      match transition with
          DF.GUse new_state -> ignore (printf " propogates %a\n" pretty new_state);
        | DF.GDefault -> ignore (printf " has no special effect\n");
        | _ -> E.s (E.bug "unexpected evaluation of guard\n")
    );
    flush stdout;
  ;;

    
  let debug_stmt (s: stmt) (transition: t DF.stmtaction) : unit =
    if (!dbg_is_store_s) then (
      ignore (printf "IsStored.DFO.doStmt: Examining statement %d (%a):\n%a\nwith effect:" 
                s.sid d_loc (get_stmtLoc s.skind) d_stmt s);
      match transition with 
          DF.SUse new_state -> ignore (printf " propogates %a\n" pretty new_state);
        | DF.SDefault -> ignore (printf " has no special effect\n");
        | DF.SDone -> ignore (printf " ends evaluation with state %a\n"
            pretty (IH.find stmtStartData s.sid));
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
   * - ReturnTaken is special case for return statements that can have "take"
   * data more than once without causing an error
   * - Everything else causes an error
   *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) = 

    debug_combine_head s new_state old;

    let transition = match (new_state, old) with
      | (Null, Null) 
      | (Taken, Taken)
      | (MustTake, MustTake)
      | (_, Error) -> None
      
      | (MustTake, Null) -> Some MustTake
      | (Null, MustTake) -> None
      
      | (Taken, Null) -> Some Taken
      | (Null, Taken) -> None

      | (MustTake, ReturnTaken) ->
          Some MustTake

      | (Null, ReturnTaken)
      | (Taken, ReturnTaken) ->
          None

      | (ReturnTaken, _) ->
          E.s (E.bug "IsStored.combinePredecessors: Hmm... Didn't think that would happen :-(\n")

      | _ -> Some Error
    in

      debug_combine_tail s transition old;
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
          E.error "IsStored.doInstr: DOUBLE store at instruction %a (%a)\n"
                    d_instr i d_loc (get_instrLoc i);
          DF.Done Error

      | (MustTake, IStore)
      | (Null, IStore) -> 
          DF.Done Taken
      
      | (ReturnTaken, _) ->
          E.s (E.bug "IsStored.doInstr: Hmm... Didn't think that would happen :-(\n")

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
          (is_field_of !target [e] s.sid) && !claim_on_return
        ) ->
          
          if (state = MustTake || state = Null || state = ReturnTaken) then (
            IH.replace stmtStartData s.sid ReturnTaken;
            DF.SDone
          ) else (
            E.error "IsStored.doInstr: DOUBLE store via return %d (%a)\n"
              s.sid d_loc (get_stmtLoc s.skind);
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
        if !dbg_is_store_g then (
          ignore (printf "Checking (target, NULL) on %a and %a: (%b, %b)\n"
                    d_exp e1 d_exp e2 is_target is_null);
        );
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

(* Called after running the dataflow for a specific target.  This function
 * checks that the dataflow state is such that at each Return statement the
 * target is claimed.
 *)
let is_stored (f: fundec) : bool =
  List.for_all 
    (fun s -> 
       try match (IH.find DFO.stmtStartData s.sid) with
           Taken | Null | ReturnTaken -> true
           | MustTake | Error -> false
       with Not_found -> true
    )
    (U.get_return_statements f)
;;


(* Helper function to initialize the dataflow state.  Notes if the function
 * returns allocated data.  
 *)
let set_claim_on_return (f: fundec) : unit =
  claim_on_return := match f.svar.vtype with
      TFun (t, _, _, _) when (hasAttribute "sos_claim" (typeAttrs t)) -> true
    | _ -> false
;;


(* Check to see if the formal variable v is stored within function f. 
 *)
let is_stored_func (e: exp) (f: fundec) (new_stores: exp list) : bool = 
  
  target := e;
  set_claim_on_return f;
  stores := new_stores;
             
  IH.clear DFO.stmtStartData;
  IH.add DFO.stmtStartData (List.hd f.sbody.bstmts).sid MustTake;
  Track.compute [List.hd f.sbody.bstmts];
 
  is_stored f  
;;


(* Check to see that the expression e allocated by instruction i is stored
 * before the end of the current function f. *)
let is_stored_instr 
      (e: exp) (s: stmt) (i: instr) (f: fundec) (new_stores: exp list) 
      : bool = 
  
  target := e;
  set_claim_on_return f;
  stores := new_stores;
             
  IH.clear DFO.stmtStartData;

  let start_state = match i with
      Set (lv, _, _) 
    | Call (Some lv, _, _, _) when (
        (* (is_field_of (Lval lv) !stores s.sid) *)
        List.exists 
          (fun store -> IE.is_equiv (Lval lv) store s.sid) 
          !stores
      ) -> 
        Taken
    | _ -> 
        MustTake
  in
       
    if !dbg_is_store_i then (
      ignore (printf "IsStored.DFO.is_store_Instr:");
      ignore (printf "Data enters as %a via %a\n" DFO.pretty start_state d_instr i);
    );
  
    List.iter 
      (fun s -> IH.add DFO.stmtStartData s.sid start_state) 
      s.succs;
    Track.compute s.succs;
     
    is_stored f
;;
  




  (* 
  (* TODO: Should this be looking for field membership? *) 
  let is_heap (e: exp) (id: int) : bool =
    List.exists
      (fun e -> match e with

           Lval (Var v, NoOffset) ->
             (* Used to cover IsEquivalent transformation *)
             (Str.string_match (Str.regexp "__heap") v.vname 0)

         | Lval (Mem (Lval (Var v, NoOffset)), NoOffset) ->
             (* Used to cover SimpleMem transformation *)
             (Str.string_match (Str.regexp "mem_") v.vname 0)

         | _ -> false
      )
      (IE.get_equiv_set e id)
  in
   *)
    (* 
     else if (
     (is_heap (Lval lv) sid) && 
     (is_field_of !target [e] sid) 
     ) then (
     ignore (printf "SUCCESS!!!\n");
     IStore
     )
     *)

