open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module A = AliasWrapper
module E = Errormsg
 
(* Current statement needed for mustAlias analysis *)
let currentStmt = ref (mkEmptyStmt ())

(* Table storing exp that have data alloced into them *)
let allocExps: exp list ref = ref []
                              
type status = Empty | Full | Error

(* Runtime debugging flags. *)
let dbg_return_combine = ref false
let dbg_return_i = ref false
let dbg_return_s = ref false
                   
(* Dataflow specific debugging. *)
let dbg_return_df = ref false
                          
                          
module DFR = struct

  (* Vital stats for this dataflow. *)
  let name = "returnFlow"
  let debug = dbg_return_df
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
          if !dbg_return_combine then
            ignore (printf "RETURN COMBINE: Join stays in Empty\n");
          None
      | (Full, Full) -> 
          if !dbg_return_combine then
            ignore (printf "RETURN COMBINE: Join stays in Full\n");
          None
      | (Error, Error) -> 
          if !dbg_return_combine then
            ignore (printf "RETURN COMBINE: Join stays in Error\n");
          None
      | _ ->
          if !dbg_return_combine then
            ignore (printf "RETURN COMBINE: Join transitions to Error\n");
          Some Error

  (* Go go data flow!  An instruction that allocates data will update the
   * allocExps table.  An instruction that stores over an exp may also update
   * this table.
   *)
  (* TODO: This is assuming that an expression never has to leave the table (ie.
   * is released twice.)  Does another part of this check cover this? 
   *)
  let doInstr (i: instr) (state: t): t DF.action = 
    
    let returnedList = U.getOwn i 
    in

      if !dbg_return_i && (List.length returnedList = 0) then
        ignore (printf "RETURN I: No data created in in struction %a\n" d_instr i);
      
      List.iter
        (fun e ->
           if not (List.mem e !allocExps) then (
             if !dbg_return_i then 
               ignore (printf "RETURN I: Adding expression %a to allocExps\n" d_exp e);
             allocExps := e::!allocExps;
             ()
           ) else (
             if !dbg_return_i then 
               ignore (printf "RETURN I: Expression %a allread in allocExps\n" d_exp e);
             ()
           )
        )
        returnedList;

      DF.Done Empty
        

  (* Can a statement take control of data? *)
  let doStmt (s: stmt) (state: t) = 
    if !dbg_return_s then
      ignore (printf "RETURN S: Examining statement %d %a\n" s.sid d_stmt s);
    currentStmt := s;
    
    match s.skind with
        Return (Some e, _) ->
          let returnMem = 
            List.exists
              (fun mem -> 
                 let b = A.mustAliasWrapper e mem s in
                   
                   if b && !dbg_return_s then (
                     ignore (printf "RETURN S: Returning allocated data: %a\n" 
                               d_exp mem);
                   ); 
                   
                   if (not b) && !dbg_return_s then (
                     ignore (printf "RETURN S: Returning data that may not be allocated: %a\n"
                               d_exp mem);
                   );
                   
                   b
              )
              !allocExps
          in
            if returnMem then (
              if !dbg_return_s then 
                ignore (printf "RETURN S: Function fills return!\n");
              IH.replace stmtStartData s.sid Full;
              DF.SDone
            ) else (
              if !dbg_return_s then 
                ignore (printf "RETURN S: Function does not fill return\n");
              IH.replace stmtStartData s.sid Error;
              DF.SDone
            )

      | _ -> 
          DF.SDefault

  let doGuard _ _ = DF.GDefault
 
  (* All blocks go on worklist. *)                                                    
  let filterStmt _ = true

end

module Track = DF.ForwardsDataFlow(DFR)

let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)


