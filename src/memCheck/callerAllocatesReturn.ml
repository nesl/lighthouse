open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module IE = IsEquivalent
module E = Errormsg
 
(* Current statement needed for mustAlias analysis *)
let currentStmt = ref (mkEmptyStmt ());;

(* Table storing exp that have data alloced into them *)
let allocExps: exp list ref = ref [];;
                              
type status = Empty | Full | Error;;

(* Runtime debugging flags. *)
let dbg_return_combine = ref false;;
let dbg_return_i = ref false;;
let dbg_return_s = ref false;;
                   
(* Dataflow specific debugging. *)
let dbg_return_df = ref false;;
                          
                          
module DFR = struct

  (* Vital stats for this dataflow. *)
  let name = "returnFlow";;
  let debug = dbg_return_df;;
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

  let debug_combine (s: stmt) (state: status) : unit =
    if !dbg_fill_combine then (
      ignore (printf "XXX.combinePredecessors: Join at statement %a" d_stmt s);
      match transition with
          Some _ -> ignore (printf "Transitions to Error\n");
        | None -> ignore (printf "Stays in %a\n" pretty old);
    )
  ;;


  let debug_instr (i: instr) (allocated_exps: exp list) : unit = 
    if !dbg_fill_i then (
      ignore (printf "XXX.doInstr: Instruction %a allocates: " d_instr i);
      List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) allocated_exps;
    )
  ;;


  let debug_stmt (returns_mem: bool) (s: stmt) : unit =
    if !dbg_return_s then (
      ignore (printf "XXX.doStmt: Statment %a " d_stmt s);
      if returns_mem then (
        ignore (printf "fills return\n");
      ) else (
        ignore (printf "does NOT fill return\n");
      )
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
      debug_combine s old;
      transition
  ;;


  (* An instruction that allocates data will update the allocExps table.  An
   * instruction that stores over or releases an exp may also update this table.
   *)
  (* TODO: Current implementation ignores this latter point and is assuming that
   * an expression never has to leave the table. 
   *)
  let doInstr (i: instr) (state: t): t DF.action = 
    
    let filled_exps = U.get_claim i in

      debug_instr i is_target_filled;
      
      List.iter
        (fun e -> if not (List.mem e !allocExps) then 
           allocExps := e :: !allocExps;)
        filled_exps;

      DF.Done Empty
  ;;
        

  
  let doStmt (s: stmt) (state: t) = 
    
    if !dbg_fill_s then ignore (printf "FILL S: Examining statement %d %a\n" s.sid d_stmt s);
    currentStmt := s;
    
    match s.skind with
        Return (Some e, _) ->
          
          let returnMem = 
            List.exists 
              (fun alloced_exp -> IE.is_equiv e alloced_exp s.sid) 
              !allocExps
          in

            debug_stmt returnMem s;
            
            if returnMem then (
              IH.replace stmtStartData s.sid Full; DF.SDone
            ) else (
              IH.replace stmtStartData s.sid Error; DF.SDone
            )

      | _ -> 
          DF.SDefault
  ;;


  let doGuard _ _ = DF.GDefault;;
 

  (* All blocks go on worklist. *)                                                    
  let filterStmt _ = true;;

end

module Track = DF.ForwardsDataFlow(DFR);;


let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)
;;


