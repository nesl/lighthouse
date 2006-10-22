open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module E = Errormsg
module IE = IsEquivalent
module MA = MayAliasWrapper
              
(* Reference to the varinfo ID that we are interested in *)
let target = ref mone;;
let freeLineNum = ref (-1);;
                
(* Reference to the current statment *)
let currentStmt = ref (mkEmptyStmt ());;

(* Dead varinfos should never appear on the rhs of an expression.  A varinfo
 * can transition into being hidden by appearing as an lval.  This may result
 * in the old value being overwritten by a new value.  From this point forward
 * that varinfo is safe to use.  Finally, an error occures if an instruction
 * attempts to use a dead lval.  *)
type status = Dead | Loops | Error;;

(* Runtime debuging flags. *)
let dbg_is_dead_s = ref false;;
let dbg_is_dead_i = ref false;;
let dbg_is_dead_c = ref false;;
let enable_loop = ref false;;
                         
(* Dataflow specific debugging *)
let dbg_free_df = ref false;;



(****************************************)
(****************************************)
(* Low layer utility functions used within this code. *)
(****************************************)
(****************************************)

(* Grab all expressions on the right hand side of an instruction. *)
let get_dereferenced_exps (i:instr) : exp list =                    
  match i with
      Set ((Var v, NoOffset), e, _) when 
        (Str.string_match (Str.regexp "__cil_tmp") v.vname 0) -> 
        (* TODO: Why is this special case needed for the __cil_tmp variabels?
         *)
        begin 
          match e with
              (* TODO: Is it okay to comment this out... *)
              (* Lval (Mem e1, NoOffset) -> [e] *)
            | _ -> []
        end
    | Set ((Mem e, _), e2, _) -> [e; e2]
    | Set (_, e, _) -> [e]
    | Call (Some (Mem e, _), _, el, _) -> e::el
    | Call (_, _, el, _) -> el
    | _ -> []
;;


(* Generate a list that includes expression e and all sub-expressions. *)
let rec sub_exps_of (e:exp) : exp list =
  match e with
      Const _ 
    | AddrOf (_, NoOffset) 
    | StartOf (_, NoOffset)
    | Lval (Var _, _) ->
        [e]

    | BinOp (_, e1, e2, _) -> 
        e :: (sub_exps_of e1) @ (sub_exps_of e2)

    | UnOp (_, e1, _)
    | CastE (_, e1) -> 
        e :: (sub_exps_of e1)

    | Lval (Mem e1, NoOffset) -> 
        e :: (sub_exps_of e1)

    | _ -> E.s (E.bug "IsDead.sub_exps_of: Running simplify should eliminate this bug!\n")
;;


(* Check to see if an instruction dereferences dead data *)
let is_dead_exp (e:exp) : bool =

  let sub_exps = sub_exps_of e in

  (* Many programmers write code of the form:
   *   ...
   *   free(ptr);
   *   ptr = NULL;
   *   ...
   * We want to allow this explicit NULLing of freed data.  So do not flag an
   * error if the target expression is being set to NULL.
   *)
  let non_null = 
    List.filter 
      (fun e -> 
         (not (IE.is_equiv e IE.nullPtr !currentStmt.sid)) && 
         (not (isZero (stripCasts e)))
      ) 
      sub_exps 
  in
   
    List.exists 
      (fun e -> 
         MA.may_alias_wrapper e !target
    ) non_null
;;



(* Check to see if an instructions is safe by insuring that it does not attempt
 * to access dead data. *)
let safe_instruction (i:instr) : status =

  let incoming_exps = get_dereferenced_exps i in

  (* If an instruction trys to use an expression that should be treated as dead,
   * then it is unsafe *)
  let unsafe = List.exists is_dead_exp incoming_exps in
  
    (* If an instruction touches a dead expression than it is unsafe *)
    if (!dbg_is_dead_i) then (
      ignore (printf "IsDead.safe_instruction: Instruction %a\n" d_instr i); 
      if unsafe then (
        ignore (printf "dereferences dead expression %a\n" d_exp !target)
      ) else (
        ignore (printf "is safe with respect to expression %a\n" d_exp !target)
      );
      flush stdout;
    );

    if (unsafe && (!freeLineNum >= (get_instrLoc i).line)) then 
      Loops
    else if (unsafe) then 
      Error
    else Dead
;;


(* Check to see if a statement treats data as being dead. *)
let safe_statement (s:stmt) : status =

  let unsafe = 
    match s.skind with
      | Return (Some e, _)
      | If (e, _, _, _)
      | Switch (e, _, _, _) -> 
          is_dead_exp e
      
      | Return _
      | Instr _
      | Goto _
      | Break _
      | Continue _
      | Loop _
      | Block _
      | TryFinally _
      | TryExcept _ -> 
          false
  in
                                   
    (* If an statement touches a dead expression than it is unsafe *)
    if (!dbg_is_dead_s) then (
      ignore (printf "IsDead.safe_statement: Statement %a\n" d_stmt s); 
      if unsafe then (
        ignore (printf "dereferences dead expression %a\n" d_exp !target)
      ) else (
        ignore (printf "is safe with respect to expression %a\n" d_exp !target)
      );
      flush stdout;
    );

    if (unsafe && (!freeLineNum >= (get_stmtLoc s.skind).line)) then 
      Loops
    else if (unsafe) then 
      Error
    else Dead
;;


(****************************************)
(****************************************)
(* Data  flow implementation *)
(****************************************)
(****************************************)
module DFD = struct

  (* Vital stats for this dataflow. *)
  let name = "deadFlow";;
  let debug = dbg_free_df;;
  type t = status;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "{%s}" (
      match state with
          Dead -> "Dead"
        | Loops -> "Loops"
        | Error -> "Error"
    )
  ;;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: status): status = state;;


  (* The Error state represtents data that is accessed when it should have been
   * treated as dead.  The Loops state is a special subset of this describing
   * data that (appears to) be accessed when it should have been treated as dead 
   * because of a loop construct in the code.
   * 
   * Loops dominates Error dominates Dead
   *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) =
    match (new_state, old) with

        (Dead, Dead)
      | (Loops, Loops)
      | (Error, Error)
      | (Dead, Loops) -> None

      | (Loops, Dead) -> Some Loops
                          
      | _ -> Some Error
  ;;


  (* Go go data flow! *)

  (* The error state is treated as a sink that never changes.  The analysis
   * reports the first location within the flow that things turn sour. *)
  let doInstr (i: instr) (state: t): t DF.action =
    match state with
        Dead -> DF.Done (safe_instruction i)
      | Loops -> DF.Default
      | Error -> DF.Default
  ;;


  (* If the statment is a Null check for the element of interest, then set the
   * "correct" expression to having state Taken. *)
  let doStmt (s: stmt) (state: t) =
    currentStmt := s;
    
    match state with
        Dead ->
          begin match (safe_statement s) with
              Dead -> DF.SDefault
            | Loops -> IH.replace stmtStartData s.sid Loops; DF.SDone
            | Error -> IH.replace stmtStartData s.sid Error; DF.SDone
          end

      | Loops 
      | Error ->
          DF.SDone

  let doGuard _ _ = DF.GDefault
            
  (* All blocks go on worklist. *)
  let filterStmt _ = true

end;;

module Track = DF.ForwardsDataFlow(DFD);;

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
*)


(* Run the data flow to generate must alias information for a function *)
let is_dead (e:exp) (s:stmt) =

  (* Set state required by the data flow *)
  target := e;
  freeLineNum := (get_stmtLoc s.skind).line;

  (* Run the data flow *) 
  IH.clear DFD.stmtStartData;
  List.iter (fun s -> IH.add DFD.stmtStartData s.sid Dead) s.succs;
  Track.compute s.succs;
    

  (* See if the data is treated as dead and, if not, if any errors are caused by
   * loops within the code. *)
  let (is_dead, no_loops) = 
    IH.fold
      (fun _ t (sd, nl) -> match t with   
           Dead -> (sd, nl)
         | Loops -> (sd, false)
         | Error -> (false, nl)
      )
      DFD.stmtStartData
      (true, true)
  in

    match (is_dead, no_loops) with
        (true, true) -> true
      | (true, false) -> if !enable_loop then false else true
      | (false, _) -> false
  
;;


