open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module IE = IsEquivalent
module E = Errormsg

(* Current statement needed for mustAlias analysis *)
let currentStmt = ref (mkEmptyStmt ());;

(* Reference to the varinfo ID that we are interested in *)
let target = ref mone;;

type status = Empty | Full | Error;;

(* Runtime debugging flags. *)
let dbg_fill_combine = ref false;;
let dbg_fill_i = ref false;;
let dbg_fill_s = ref false;;

(* Dataflow specific debugging. *)
let dbg_fill_df = ref false;;

module DFF = struct

  (* Vital stats for this dataflow. *)
  let name = "fillFlow";;
  let debug = dbg_fill_df;;
  type t = status;;

  (* Basic util functions to jumpstart dataflow. *)           
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "{%s}" ( 
      match state with 
          Empty -> "Empty"
        | Full -> "Full"
        | Error -> "Error"
    );;

  (********************)
  (********************)
  (* Helper functions for debugging *)
  (********************)
  (********************)

  let debug_combine (state: status) : unit =
    if !dbg_fill_combine then (
      ignore (printf "CallerAllocates.DFF.combinePredecessors: ");
      match transition with
          Some _ -> ignore (printf "Join transitions to Error\n");
        | None -> ignore (printf "Join stays in %a\n" pretty old);
    )
  ;;


  let debug_instr (i: instr) (is_filled: bool) : unit = 
    if !dbg_fill_i then (
      ignore (printf "CallerAllocates.DFF.doInstr: Expression %a " d_exp !target);
      if is_filled then
        ignore (printf "filled in instruction\n%a\n" d_instr i)
      else
        ignore (printf "NOT filled in instruction\n%a\n" d_instr i)
    )
  ;;



  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: status): status = state;;


  (* If any predecessor is in an error state, the error propigates forward. *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) = 
    let transition = match (new_state, old) with
        (Empty, Empty) | (Full, Full) | (Error, Error) -> None
        | _ -> Some Error
    in
      debug_combine old;
      transition
  ;;


  (* An instruction can cause a state to transition from Empty to Full if it
   * stores allocated data into the target. *)
  let doInstr (i: instr) (state: t): t DF.action = 

    let filled_exps = U.get_claim i in

    let is_target_filled =
      List.exists (fun e -> IE.is_equiv e !target !currentStmt.sid) filledList
    in

      debug_instr i is_target_filled;

      match (filled, state) with
          (true, Empty) 
        | (false, Full) -> DF.Done Full
        | (false, Empty) -> DF.Done Empty
        | _ -> DF.Done Error
  ;;

  (* Can a statement take control of data? *)
  let doStmt (s: stmt) (state: t) = 
    if !dbg_fill_s then ignore (printf "FILL S: Examining statement %d %a\n" s.sid d_stmt s);
    currentStmt := s;
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
let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)
;;
 *)

let lval_is_allocated (v: varinfo) (f: fundec) = 
            
  targets := [Lval (var v)];
  IH.clear DFF.stmtStartData;
  IH.add DFF.stmtStartData (List.hd f.sbody.bstmts).sid Empty;
  Track.compute [List.hd f.sbody.bstmts];

  let (error, full) = 
    IH.fold
      (fun _ t (error, taken) -> match t with   
           Empty -> (error, taken)
         | Full -> (error, true)
         | Error -> (true, taken)
      )
      DFF.stmtStartData
      (false, false)
  in

    match (error, full) with
        (false, true) -> true
      | _ -> false
;;

