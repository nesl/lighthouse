open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module IE = IsEquivalent
module E = Errormsg
 
(* Current statement needed for mustAlias analysis *)
let currentStmt = ref (mkEmptyStmt ())

(* Reference to the varinfo ID that we are interested in *)
let targets: exp list ref = ref []
                              
type status = Empty | Full | Error

(* Runtime debugging flags. *)
let dbg_fill_combine = ref false
let dbg_fill_i = ref false
let dbg_fill_s = ref false
                   
(* Dataflow specific debugging. *)
let dbg_fill_df = ref false
                          
                          
module DFF = struct

  (* Vital stats for this dataflow. *)
  let name = "fillFlow"
  let debug = dbg_fill_df
  type t = status

  (* Basic util functions to jumpstart dataflow. *)           
  let stmtStartData: t IH.t = IH.create 17
  let copy (state: t) = state
  let pretty () (state: t) =
    dprintf "{%s}" ( 
      match state with 
          Empty -> "Empty"
        | Full -> "Full"
        | Error -> "Error"
    )

  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: status): status = state

  (* If any predecessor is in an error state, the error propigates forward. *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) = 
    match (new_state, old) with
        (Empty, Empty) -> 
          if !dbg_fill_combine then
            ignore (printf "FILL COMBINE: Join stays in Empty\n");
          None
      | (Full, Full) -> 
          if !dbg_fill_combine then
            ignore (printf "FILL COMBINE: Join stays in Full\n");
          None
      | (Error, Error) -> 
          if !dbg_fill_combine then
            ignore (printf "FILL COMBINE: Join stays in Error\n");
          None
      | _ ->
          if !dbg_fill_combine then
            ignore (printf "FILL COMBINE: Join transitions to Error\n");
          Some Error

  (* CHECK Also we may be able to use this as the pre condition checker at any
   * release point... *)
  (* Go go data flow!  An instruction can cause a state to transition from
   * Empty to Full if it stores allocated data into the target.
   *)
  let doInstr (i: instr) (state: t): t DF.action = 
    
    let filledList = U.getOwn i 
    in
  
    let filled =
      List.exists
        (fun target ->
           List.exists
             (fun e -> IE.is_equiv e target !currentStmt.sid)
             filledList
        )
        !targets
    in

    if !dbg_fill_i then (
        if filled then
          ignore (printf "FILL I: Formal var filled in instruction\n%a\n" d_instr i)
        else
          ignore (printf "FILL I: Formal var NOT filled in instruction\n%a\n" d_instr i)
    );

    match (filled, state) with
        (true, Empty) -> DF.Done Full
      | (false, Empty) -> DF.Done Empty
      | (false, Full) -> DF.Done Full
      | _ -> DF.Done Error


  (* Can a statement take control of data? *)
  let doStmt (s: stmt) (state: t) = 
    if !dbg_fill_s then
      ignore (printf "FILL S: Examining statement %d %a\n" s.sid d_stmt s);
  
    currentStmt := s;
    DF.SDefault

  let doGuard _ _ = DF.GDefault
      
  (* All blocks go on worklist. *)                                                    
  let filterStmt _ = true

end

module Track = DF.ForwardsDataFlow(DFF)

let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)


