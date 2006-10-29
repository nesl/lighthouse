open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module IE = IsEquivalent
module E = Errormsg

(* Runtime debugging flags. *)
let dbg_caller_allocates_c = ref false;;
let dbg_caller_allocates_i = ref false;;
let dbg_caller_allocates_s = ref false;;

(* Dataflow specific debugging. *)
let dbg_fill_df = ref false;;

module DFF = struct

  (* Vital stats for this dataflow. *)
  let name = "fillFlow";;
  let debug = dbg_fill_df;;
  type t = exp list;;

  (* Basic util functions to jumpstart dataflow. *)           
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "{%s}" ( 
      List.fold_left 
        (fun s e -> s ^ (sprint ~width:40 (dprintf "%a " d_exp e))) 
        "" 
        state
    );;

  (********************)
  (********************)
  (* Helper functions for debugging *)
  (********************)
  (********************)

  let debug_combine (state: t) (transition: t option): unit =
    if !dbg_caller_allocates_c then (
      ignore (printf "CallerAllocatesLval.DFF.combinePredecessors: ");
      match transition with
          Some _ -> ignore (printf "Join updates to:\n%a\n" pretty state);
        | None -> ignore (printf "Join stays in:\n%a\n" pretty state);
    );
    flush stdout;
    ()
  ;;


  let debug_instr (i: instr) (state: t) : unit = 
    if !dbg_caller_allocates_i then (
      ignore (printf 
                "CallerAllocatesLval.DFF.doInstr: Instruction %a results in state:\n" 
                d_instr i);
      ignore (printf "%a\n" pretty state);
      flush stdout;
    );
    ()
  ;;


  let union_lists (el1:exp list) (el2:exp list) : exp list =
    U.sort_and_uniq (el1 @ el2)
  ;;



  let intersect_lists (el1:exp list) (el2:exp list) : exp list =
    
    let intersection = 
      List.fold_left 
        (fun el e -> if (List.mem e el1) then e::el else el)
        []
        el2
    in
      
      List.sort compare intersection
  ;;



  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: t): t = state;;


  (* If any predecessor is in an error state, the error propigates forward. *)
  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) = 
    
    let transition = 
      if (compare old new_state = 0) then 
        None
      else 
        (Some (intersect_lists old new_state))
    in
      
      debug_combine old transition;
      transition
  ;;


  (* An instruction can cause a state to transition from Empty to Full if it
   * stores allocated data into the target. *)
  (* TODO: Current implementation ignores this latter point and is assuming that
   * an expression never has to leave the table.  Unit tests that demonstarte
   * this:
   *
   * - overwrite allocated data
   *
   * - release allocated data
   *)
  let doInstr (i: instr) (state: t): t DF.action = 

    let filled_exps = U.get_claim i in
    let new_state = union_lists state filled_exps in

      debug_instr i new_state;
      DF.Done new_state
  ;;

  (* Can a statement take control of data? *)
  let doStmt (s: stmt) (state: t) = 
    if !dbg_caller_allocates_s then 
      ignore (printf "CallerAllocatesLval.Dff.doStmt: Examining statement %d %a\n" 
                s.sid d_stmt s);
    DF.SDefault
  ;;

  
  let doGuard _ _ = DF.GDefault;;
  
  
  (* All blocks go on worklist. *)                                                    
  let filterStmt _ = true;;

end

module Track = DF.ForwardsDataFlow(DFF);;

(****************************************)
(****************************************)
(* Interface Functions *)
(****************************************)
(****************************************)


(* Low layer interface to analysis inforamation.  Not currently used. *)
(*
let getStmtState (data: exp list IH.t) (s: stmt): exp list option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)
;;
 *)

let run_dataflow (f: fundec) : unit =
  IH.clear DFF.stmtStartData;
  IH.add DFF.stmtStartData (List.hd f.sbody.bstmts).sid [];
  Track.compute [List.hd f.sbody.bstmts];
  ()
;;


let var_is_allocated (v: varinfo) (f: fundec) = 

  run_dataflow f;

  List.for_all 
    (fun s ->
       let claim_list = IH.find DFF.stmtStartData s.sid in
         List.exists
           (fun e -> IE.is_equiv (Lval (var v)) e s.sid)
           claim_list
    )
    (U.get_return_statements f)
  
;;


let return_is_allocated (f: fundec) : bool = 
    
  run_dataflow f;  

  List.for_all 
    (fun s -> 
       match s.skind with
         Return (Some return, _) ->
             List.exists
               (fun e -> 
                  IE.is_equiv return e s.sid)
               (IH.find DFF.stmtStartData s.sid)
       | _ -> false 
    )
    (U.get_return_statements f)
  
;;

