open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module U = MemUtil
module A = AliasWrapper
module E = Errormsg

(* Reference to the varinfo ID that we are interested in *)
let targets = ref []
let freeLineNum = ref (-1)
                
(* Reference to the current statment *)
let currentStmt = ref (mkEmptyStmt ())

(* Dead varinfos should never appear on the rhs of an expression.  A varinfo
 * can transition into being hidden by appearing as an lval.  This may result
 * in the old value being overwritten by a new value.  From this point forward
 * that varinfo is safe to use.  Finally, an error occures if an instruction
 * attempts to use a dead lval.  *)
type status = Dead | Loops | Error

(* Runtime debuging flags. *)
let dbg_free_dead_s = ref false
let dbg_free_dead_i = ref false
let dbg_free_combine = ref false
let enable_loop = ref false
                         
(* Dataflow specific debugging *)
let dbg_free_df = ref false

(* Check to see if an instructions treats data as being dead.*)
let stillDeadI i =

  (* Bundle together all incoming expressions *)
  let incoming_exps: exp list  =
    match i with
        Set ((Var v, NoOffset), e, _) when 
          (Str.string_match (Str.regexp "__cil_tmp") v.vname 0) -> 
          begin
           match e with
             | Lval (Mem e1, NoOffset) -> [e]
             | _ -> []
          end
      | Set (lv, e, _) -> [e]
      | Call (Some lv, _, el, _) -> el
      | Call (None, _, el, _) -> el
      | _ -> []
  in

  (* CHECK *)
  (* Extract those expressions that could be of interest in the analysis.
   * This requires that some complex expressions be "traversed" so that
   * "sub-expressions" are included. *)
  let exps =
    List.fold_left
      (fun exps e ->

         let rec expressions e =
           match e with
               Const c -> [e]
             | AddrOf (v, NoOffset) -> [e]
             | StartOf (v, NoOffset) -> [e]
             | BinOp (_, e1, e2, _) -> (expressions e1) @ (expressions e2)
             | UnOp (_, e1, _) -> expressions e1
             | CastE (_, e1) -> expressions e1
             | Lval (Mem e1, NoOffset) -> e::(expressions e1)
             | Lval (Var v, o) -> [e]

             | _ -> E.bug "Running simplify should eliminate this bug!\n"; []
         in

           (expressions e) @ exps
      )
      []
      incoming_exps
  in

  (* See if any of these expressions are in the list of expressions that must
   * be treated as dead. *)
  let notDead =
    List.exists
      (fun e1 ->
         List.exists
           (fun target -> 
              let may = A.mayAliasWrapper e1 target in
              let null = A.mustAliasNull e1 !currentStmt in
                if (!dbg_free_dead_i) then (
                  match (may, null) with
                      (_, true) ->
                        ignore (printf "DEAD INSTR: Expression %a must alias NULL\n" 
                                  d_exp e1);
                    | (true, _) ->
                        ignore (printf "DEAD INSTR: Expression %a may alias target %a:\n" 
                                  d_exp e1 d_exp target);
                    | (false, _) ->
                        ignore (printf "DEAD INSTR: Expression %a may NOT alias target %a:\n" 
                                  d_exp e1 d_exp target);
                );
                (may && (not null))
           )
           !targets)
      exps
  in

    if notDead then
      (
        match i with
            Set (_, _, loc) 
          | Call (_, _, _, loc)
              when !freeLineNum >= loc.line ->
              if !dbg_free_dead_i then
                ignore (printf "DEAD INSTR: Looped instruction uses a dead expression: %a\n" 
                          d_instr i);
              if !enable_loop then
                ignore(E.warn "Dead var in %a\nappears to be accessed in a loop" d_instr i);
              Loops
          | _ ->
              if !dbg_free_dead_i then
                ignore (printf "DEAD INSTR: Instruction uses a dead expression: %a\n" 
                          d_instr i);
              ignore(E.warn "Var in %a\nshould be treated as dead" d_instr i);
              Error
      ) else (
        if !dbg_free_dead_i then
          ignore (printf "DEAD INSTR: Instruction is safe: %a\n" d_instr i);
        Dead
      )


(* CHECK *)
(* Check to see if a statement treats data as being dead. *)
let stillDeadS s =
  match s.skind with
    | Return (Some e1, loc)
    | If (e1, _, _, loc)
    | Switch (e1, _, _, loc) ->

        let notDead = 
          List.exists
            (fun target -> A.mayAliasWrapper e1 target)
            !targets
        in

          if notDead then
            (
              if !freeLineNum >= loc.line then (
                if !dbg_free_dead_s then
                  ignore (printf "DEAD STMT: Looped statement uses a dead expression: %a\n" 
                            d_stmt s);
                ignore(E.warn "Dead var in %a\nappears to be accessed in a loop" d_stmt s);
                Loops
              ) else (
                if !dbg_free_dead_s then
                  ignore (printf "DEAD STMT: Statement uses a dead expression: %a\n" 
                            d_stmt s);
                ignore(E.warn "Var in %a\nshould be treated as dead" d_stmt s);
                Error
              )
            ) else (
              if !dbg_free_dead_s then
                ignore (printf "DEAD STMT: Statement is safe: %a\n" d_stmt s);
              Dead
            )

    | Return _
    | Instr _
    | Goto _
    | Break _
    | Continue _
    | Loop _
    | Block _
    | TryFinally _
    | TryExcept _ -> Dead


module DFD = struct

  (* Vital stats for this dataflow. *)
  let name = "deadFlow"
  let debug = dbg_free_df
  type t = status

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17
  let copy (state: t) = state
  let pretty () (state: t) =
    dprintf "{%s}" (
      match state with
          Dead -> "Dead"
        | Loops -> "Loops"
        | Error -> "Error"
    )

  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: status): status = state

  (* If any predecessor is in an error state, the error propigates forward.
   * This indicates that the dead data was accessed at some point earlier in
   * the program.  Loops are between dead and error. *)
  let combinePredecessors (s: stmt) ~(old: status) (new_state: status) =
    match (new_state, old) with
        (Dead, Dead) -> None

      | (Loops, Dead) -> Some Loops

      | (Dead, Loops)
      | (Loops, Loops) -> None

      | (Error, Error) -> None
                          
      | _ -> Some Error

  (* Go go data flow!
   * An instruction can cause a state to transition from dead into an error
   * state.  The error state is a sink that never changes.
   *)
  let doInstr (i: instr) (state: t): t DF.action =
   
    (* We may as well let the entire analysis run.  Performance is not a
     * concern. SCRATCH THAT!  One mistage (especially in a loop) tends to
     * indicate many pts of error.  This overwhelms user.  We will just give
     * them a little output at a time. *) 
    match state with
        Dead -> DF.Done (stillDeadI i)
      | Loops -> DF.Done Loops
      | Error -> DF.Done Error


  (* If the statment is a Null check for the element of interest, then set the
   * "correct" expression to having state Taken. *)
  let doStmt (s: stmt) (state: t) =
    currentStmt := s;
    
    match state with
        Dead ->
          begin
            match (stillDeadS s) with
                Dead -> 
                  DF.SDefault
              | Loops -> 
                  IH.replace stmtStartData s.sid Loops;
                  DF.SDone
              | Error ->
                  IH.replace stmtStartData s.sid Error;
                  DF.SDone
          end

      | Loops 
      | Error ->
          DF.SDone

  let doGuard _ _ = DF.GDefault
            
  (* All blocks go on worklist. *)
  let filterStmt _ = true

end

module Track = DF.ForwardsDataFlow(DFD)

let getStmtState (data: status IH.t) (s: stmt): status option =
  try Some (IH.find data s.sid)
  with Not_found -> None (* Assume that data is not taken *)


