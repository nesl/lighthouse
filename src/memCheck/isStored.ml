open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module IE = IsEquivalent
module E = Errormsg
 
(* Blah... *)             
let currentStmt = ref (mkEmptyStmt ())
let currentFunc = ref None
                    
(* Reference to the varinfo ID that we are interested in *)
let target: exp ref = ref mone
let stores: exp list ref = ref []
                              
type status = MustTake | Taken | Error | Null | IfNull | ReturnTaken

(* Runtime debugging flags. *)
let dbg_alloc_store_i = ref false
let dbg_alloc_store_s = ref false
let dbg_alloc_combine = ref false
 
(* Dataflow specific debugging. *)
let dbg_alloc_df = ref false
                          
                          
(* Check to see if an instructions takes data. *)

(* Ways to take (and loose) data: 
 * - use a set instruction to save data pointer into a presistent store (taken)
 * - use a set instruction to overied the data pointer (error)
 * - call a function that frees the pointer to the data (taken)
 * - call a function that destroys the pointer to the data (error)
 * *)

(* TODO: This will not detect if allocated data is stored directly into a
 * store (ie. "store = malloc(5);") *) 

let takenI i s = 

  (* Note is instruction saves data into a store *)  
  let isStore = 
    match i with
        Set (lv, _, _) 
      | Call (Some lv, _, _, _) ->
          List.exists 
            (fun store -> 
               IE.is_equiv (Lval lv) store s.sid)
            !stores
      | _ -> false
  in

  (* Note if instruction saves target data *)
  let isTarget =
    match i with
        Set (_, e, _) -> 
          let b = IE.is_equiv e !target s.sid in
            if (!dbg_alloc_store_i) then (
              if b then 
                ignore (printf "ALLOC STORE I: Expression %a matches target %a!\n"
                          d_exp e d_exp target)
              else
                ignore (printf "ALLOC STORE I: Expression %a does not match target %a\n"
                          d_exp e d_exp target)
            );
            b
      | _ -> false
  in

  (* Note if instruction releases target data *)
  let isReleased =
    match i with
        Call (_, _, el, _) -> 
          List.exists
            (fun release -> IE.is_equiv release !target s.sid)
            (U.get_released i)
      | _ -> false
  in

    (* Combine the above information to determine what the instruction does *)
    match (isStore, isTarget, isReleased) with
        (true, true, false) ->
          if (!dbg_alloc_store_i) then
            ignore (printf 
                      "ALLOC STORE I: Taken in instruction %a\n" 
                      d_instr i);
          true
      
      | (false, true, true) 
      | (true, false, true) 
      | (false, false, true) ->
          if (!dbg_alloc_store_i) then
            ignore (printf 
                      "ALLOC STORE I: Released in instruction %a\n" 
                      d_instr i);
          true
      
      |  (true, true, true) ->
          ignore (E.bug 
            "Instruction should not both release and store data: %a\n" 
            d_instr i);
          false
      
      | (true, false, false) 
      | (false, true, false) 
      | (false, false, false) ->
          if (!dbg_alloc_store_i) then
            ignore (printf 
                      "ALLOC STORE I: (%b %b %b) Not effected by instruction %a\n" 
                      isStore isTarget isReleased d_instr i);
          false

 
module DFO = struct

  (* Vital stats for this dataflow. *)
  let name = "ownFlow"
  let debug = dbg_alloc_df
  type t = status

  (* Basic util functions to jumpstart dataflow. *)           
  let stmtStartData: t IH.t = IH.create 17
  let copy (state: t) = state
  let pretty () (state: t) =
    dprintf "{%s}" ( 
      match state with 
          MustTake -> "Must Take"
        | Taken -> "Taken"
        | Null -> "Null"
        | IfNull -> "IfNull"
        | Error -> "Error"
        | ReturnTaken -> "ReturnTaken"
    )
  ;;

  
  let debug_combine (s: stmt) (transition: t option) (old_state: t): unit =
    if !dbg_fill_combine then (
      ignore (printf "XXX.combinePredecessors: Join at statement %a" d_stmt s);
      match transition with
          Some state -> ignore (printf "Transitions to %a\n" pretty state);
        | None -> ignore (printf "Stays in %a\n" pretty old);
    )
  ;;


  let debug_instr (s: stmt) (transition: t option) (old_state: t): unit =
    if !dbg_fill_combine then (
      ignore (printf "XXX.combinePredecessors: Join at statement %a" d_stmt s);
      match transition with
          Some state -> ignore (printf "Transitions to %a\n" pretty state);
        | None -> ignore (printf "Stays in %a\n" pretty old);
    )
  ;;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: status): status = state

  (* If any predecessor is in an error state, the error propigates forward.
   * This indicates that the dead data was accessed at some point earlier in
   * the program. *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) = 
    let transition = match (new_state, old) with

      | (Null, Null) 
      | (Taken, Taken)
      | (MustTake, MustTake)
      | (ReturnTaken, ReturnTaken)
      | (Error, Error) -> None
      
      | (MustTake, IfNull) -> Some Null
          
      | (Taken, IfNull) -> Some Taken
    
      | (MustTake, Null) -> Some MustTake
      
      | (Null, MustTake) -> None

      | (Null, Taken) -> None

      | (Taken, Null) -> Some Taken

      | (Null, ReturnTaken)
      | (Taken, ReturnTaken) -> None 

      | (MustTake, ReturnTaken) -> Some MustTake

      | (ReturnTaken, Null)
      | (ReturnTaken, Taken) -> Some ReturnTaken

      | _ -> Some Error
    in

      debug_combine s transition old;
      transition
  ;;


  (* Go go data flow!  An instruction can cause a state to transition from dead
   * to any of: dead, hidden, or error.  The hidden state can only transition as
   * the result of a merge.  The NULL state simply stays NULL until it meets a
   * join point.  The error state is a sink that never changes.
   *)
  let doInstr (i: instr) (state: t): t DF.action = 
    let transition = match (state, (takenI i !currentStmt)) with
        (MustTake, true) -> Some Taken
      | (MustTake, false) -> None
      | (Taken, true) -> 
          ignore (E.warn "Alloced data %a may be stored a second time in %a\n" 
                    d_exp !target d_instr i);
            None
      | (Taken, false) -> None
      | (Null, _) -> None
      | (Error, _) -> None
      | (IfNull, _) -> Some Error
      | (ReturnTaken, _) -> Some Error
    in

      debug_instr s transition state;
      match transition with 
          Some new_state -> DF.Done new_state
        | None -> DF.Done state
  ;;


  (* Can a statement take control of data?  Yes, it can by returning the data
   * provided that the return type of the function is annotated as sos_claim. It
   * can also "take control" of data by establishing that a malloc attempt failed
   * by performing a NULL check. 
   * *)
  let doStmt (s: stmt) (state: t) = 
    if (!dbg_alloc_store_s) then (
      ignore (printf "ALLOC STORE S: Examining statement %d %a:\n" 
                s.sid d_stmt s);
      ignore (printf "ALLOC STORE S: ....with current state %a\n" 
                pretty state);
    );
    currentStmt := s;

    match s.skind with 
        Return (Some e, _) ->
          
          let returnsTarget = IE.is_equiv e !target s.sid in

          let releaseAttribute =
            match !currentFunc with
                Some f ->
                 begin
                   match f.svar.vtype with
                       TFun (t, _, _, _) when (hasAttribute "sos_claim" (typeAttrs t)) ->
                         true
                     | _ -> 
                         false
                       
                 end 
              | _ -> E.s (E.error "Function must be set before calling ownFlow\n")
          in

            (
              match (state, returnsTarget, releaseAttribute) with
                  (MustTake, true, true)
                | (Null, true, true) 
                | (Taken, true, false)
                | (Taken, false, true)
                | (Taken, false, false)
                | (Null, true, false)
                | (Null, false, true)
                | (Null, false, false) ->
                    if (!dbg_alloc_store_s) then
                      ignore (printf "ALLOC STORE S: Return in ReturnTaken\n");
                    IH.replace stmtStartData s.sid ReturnTaken;
                    DF.SDone
                | _ -> 
                    if (!dbg_alloc_store_s) then
                      ignore (printf "ALLOC STORE S: Return in %a\n" pretty state);
                    DF.SDone
            )
        
      | Return (None, _) ->
          begin
            match state with
                Taken ->
                  if (!dbg_alloc_store_s) then
                    ignore (printf "ALLOC STORE S: Return in ReturnTaken\n");
                  IH.replace stmtStartData s.sid ReturnTaken;
                  DF.SDone
              | _ -> 
                  if (!dbg_alloc_store_s) then
                    ignore (printf "ALLOC STORE S: Return in %a\n" pretty state);
                  DF.SDone
          end
      
      (*
       * Test to see if the pointer is NOT "null".  In these checks the "if"
       * block must treat the pointer as being alive (MustTake or Taken), while
       * the "else" block can treat the pointer as being Null.  The block after
       * the If statement will have a Null state flowing into it.
       * *)   

      (* Unary check to see if item is NOT null *)

      | If (Lval lv, b1, _, _) ->

          let e1Target = IE.is_equiv (Lval lv) !target s.sid in
            
          if e1Target then (
            
            if (!dbg_alloc_store_s) then
              ignore (printf "ALLOC STORE S: Data Null in else clause and after check\n");
          
            List.iter 
              (fun next ->
                 if next.sid != (List.nth b1.bstmts 0).sid then (
                   if (!dbg_alloc_store_s) then
                     ignore (printf "ALLOC STORE S: Setting %d to IfNull\n" next.sid);
                   IH.replace stmtStartData next.sid IfNull;
                 );
              )
              s.succs;
          );

          DF.SDefault
            
            
      (* Binary check to see if item is NOT null *)

      | If (BinOp (Ne, e1, e2, _), b1, _, _) ->
         
          let e1Null = 
            IE.is_equiv e1 IE.nullPtr s.sid
          in

          let e2Null = 
            IE.is_equiv e2 IE.nullPtr s.sid
          in

          let e1Target = IE.is_equiv e1 !target s.sid in

          let e2Target = IE.is_equiv e2 !target s.sid in

          let b1Statements = 
            (List.length b1.bstmts) > 0
          in
            
          let nullCheck = 
            (e1Null && e2Target) || (e2Null && e1Target)
          in
            
            if (nullCheck) then (
              
              if (!dbg_alloc_store_s) then
                ignore (printf "ALLOC STORE S: Data Null in else clause and after check\n");
          
              List.iter 
                (fun next ->
                   if next.sid != (List.nth b1.bstmts 0).sid then (
                     if (!dbg_alloc_store_s) then
                       ignore (printf "ALLOC STORE S: Setting %d to IfNull\n" next.sid);
                     IH.replace stmtStartData next.sid IfNull;
                   );
                )
                s.succs;
            );

            DF.SDefault
         

      (* Test to see if the point IS "null".  In these checkes the "if" block
       * can treat the pointer as Null, while the "else" block must treat the
       * pointer as being alive (MustTake or Taken). 
       * *)
      
      (* Unary check to see if item is Null *)

      | If (UnOp (LNot, (Lval lv), _), b1, _, _) ->
              
          let e1Target = IE.is_equiv (Lval lv) !target s.sid in

          let b1Statements = 
            (List.length b1.bstmts) > 0
          in

            if (e1Target) then (
              
              if (!dbg_alloc_store_s) then
                ignore (printf "ALLOC STORE S: Data Null in if clause\n");

              if b1Statements then
                IH.replace stmtStartData (List.nth b1.bstmts 0).sid IfNull;
            
            );

            DF.SDefault

            
      (* Binary check to see if item is Null *)

      | If (BinOp (Eq, e1, e2, _), b1, _, _) ->
         
          let e1Null = 
            IE.is_equiv e1 IE.nullPtr s.sid
          in

          let e2Null = 
            IE.is_equiv e1 IE.nullPtr s.sid
          in

          let e1Target = IE.is_equiv e1 !target s.sid in

          let e2Target = IE.is_equiv e2 !target s.sid in

          let b1Statements = 
            (List.length b1.bstmts) > 0
          in
            
          let nullCheck = 
            (e1Null && e2Target) || (e2Null && e1Target)
          in
            
            if (!dbg_alloc_store_s) then (
              ignore (printf "ALLOC STORE S: Null data check for statement %a:\n" d_stmt s);
              ignore (printf "#### E1: Null -> %b Target -> %b, E2: Null -> %b Target %b\n"
                        e1Null e1Target e2Null e2Target);
            );

            
            
            if (nullCheck) then (
              
              if (!dbg_alloc_store_s) then
                ignore (printf "ALLOC STORE S: Data Null in if clause\n");

              if b1Statements then
                IH.replace stmtStartData (List.nth b1.bstmts 0).sid IfNull;
            
            );

            DF.SDefault
      | _ -> 
          DF.SDefault

  let doGuard _ _ = DF.GDefault
  
  (* All blocks go on worklist. *)                                                    
  let filterStmt _ = true

end

module Track = DF.ForwardsDataFlow(DFO)

let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)


