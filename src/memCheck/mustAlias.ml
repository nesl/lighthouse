open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module E = Errormsg
module U = MemUtil

(* Runtime debuging flags. *)
let dbg_must_i = ref false;;
let dbg_must_combine = ref false;;

(* Dataflow specific debugging *)
let dbg_must_df = ref false;;

type alias = Next of exp | End | Null | Dead;;

type must_table = (exp, alias) Hashtbl.t;;

let print_must_table (table:must_table) =
  Hashtbl.iter
    (fun (e:exp) (a:alias) -> 
       ignore (printf "Exp %a -> " d_exp e);
       match a with
           Next e -> ignore (printf "%a\n" d_exp e)
         | End -> ignore (printf "End\n")
         | Null -> ignore (printf "Null\n")
         | Dead -> ignore (printf "Dead\n") 
    )
    table
;;
                   
module DFM = struct

  (* Vital stats for this dataflow. *)
  let name = "mustFlow";;
  let debug = dbg_must_df;;
  type t = must_table;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = Hashtbl.copy state;;
  let pretty () (state: t) =
    dprintf "{%s}" ( "Print not implemented..."
    );;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: t) = state;;

  
  (* Merge points take the intersection of the two sets *)
  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) : t option =  

    (* Print incoming state *)
    if (!dbg_must_combine) then (
      ignore (printf "MUST COMBINE: Examining State: %d:\n" s.sid);
      ignore (printf "MUST COMBINE: Incoming old state:\n");
      print_must_table old;
      ignore (printf "MUST COMBINE: Incoming merge:\n");
      print_must_table new_state;
      flush stdout;
    );

    (* Create a NEW state by merging the two old state.  Be careful since OCaml
     * defaults to using references to copy a hash table...
     *)
    let state = Hashtbl.create 5 in
      
      (* For each element in old state state, if it has the same alias good,
       * else drop it *)
      Hashtbl.iter 
        (fun v a -> 
            try 
              if (Util.equals (Hashtbl.find new_state v) a) then
                Hashtbl.add state v a
              else
                Hashtbl.add state v Dead
            with _ -> Hashtbl.add state v Dead)
        old;
     
      (* Print outgoing state *)
      if (!dbg_must_combine) then (
        ignore (printf "MUST COMBINE: Post merge:\n");
        print_must_table state;
      );

      (* Check to see if the state is the same INDEPENDENT of the order of
       * entries by checking that:
       *   - all enteries is the old state are in the generated state AND
       *   - all enteries in the generated state are in the old state
       *)
      if (
        try
          let seo =
            (Hashtbl.fold
               (fun e a b -> b && (Util.equals (Hashtbl.find old e) a))
               state
               true)
          in
          let soe = 
            (Hashtbl.fold
               (fun e a b -> b && (Util.equals (Hashtbl.find state e) a))
               old
               true)
          in
            seo && soe
        with
            _ -> false
      ) then (
        None
      ) else (
        Some state
      )


  (* Go go data flow!
   * This is overly conservative since it only handles situations such as:
   *    a = b;
   *  but fails on items such as:
   *    a = &b;
   *    *a = b;
   *)
  let doInstr (i: instr) (state: t): t DF.action =
    match i with

        Set (lv, e, _) when (Util.equals e zero) ->
          Hashtbl.replace state (Lval lv) Null;
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n maps expr %a to Null\n" d_instr i d_exp (Lval lv));
          DF.Done state
      
      | Set (lv, e, _) when (isConstant e) || (match isInteger e with None -> false | _ -> true) ->
          Hashtbl.replace state (Lval lv) End;
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n maps exp %a to End\n" d_instr i d_exp (Lval lv));
          DF.Done state

      | Set (lv, e, _) -> begin match e with
            Lval lv2 ->
              Hashtbl.replace state (Lval lv) (Next (Lval lv2));
              if (!dbg_must_i) then
                ignore (printf "MUST I: %a\n maps exp %a to %a\n" d_instr i d_exp (Lval lv) d_exp (Lval lv2));
              DF.Done state

          | CastE (_, Lval lv2) ->
              Hashtbl.replace state (Lval lv) (Next (Lval lv2));
              if (!dbg_must_i) then
                ignore (printf "MUST I: %a\n maps exp %a to %a\n" d_instr i d_exp (Lval lv) d_exp (Lval lv2));
              DF.Done state

          | _ -> 
              E.warn "mustFlow doInstr: Do not understand RHS of instrurtion %a.  Skipping.\n" d_instr i;
              DF.Done state
        end
      
      | Call (Some lv, _, _, _) ->
          Hashtbl.replace state (Lval lv) End;
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n maps exp %a to End\n" d_instr i d_exp (Lval lv));
          DF.Done state

      | _ -> 
          (* TODO: Remove this after debugging and replace with version
          * commented out below. *)
          E.warn "mustFlow doInstr: Ignoring instruction %a\n" d_instr i;
          DF.Done state
          (*
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n is ignored\n" d_instr i);
          DF.Done state
           *)

  let doGuard _ _ = DF.GDefault


  (* Statements should not effect alias information *) 
  let doStmt (s: stmt) (state: t) = DF.SUse (Hashtbl.copy state)

  (* All blocks go on worklist. *)
  let filterStmt _ = true

end

module TrackF = DF.ForwardsDataFlow(DFM)

let get_stmt_state (data: must_table IH.t) (s: stmt): must_table option =
  try Some (IH.find data s.sid)
  with Not_found -> None 

let get_id_state (data: must_table IH.t) (id: int): must_table option =
  try Some (IH.find data id)
  with Not_found -> None 

let print_alias (id:int) =
  match (get_id_state DFM.stmtStartData id) with
      Some table -> 
        ignore (printf "\n\nState %d:\n" id);
        Hashtbl.iter 
          (fun key value ->
             ignore (printf "%a (%d) -> " d_exp key (Hashtbl.hash key));
             match value with
                 Next e -> ignore (printf "%a (%d)\n" d_exp e (Hashtbl.hash e))
               | End -> ignore (printf "End\n")
               | Null -> ignore (printf "Null\n")
               | Dead -> ignore (printf "Dead\n")
          )
          table;
        ()
    | None ->
        ignore (printf "\n\nUnable to find state %d\n" id);
;;
        
let generate_must_alias (f:fundec) =
  IH.clear DFM.stmtStartData;
  IH.add DFM.stmtStartData (List.hd f.sbody.bstmts).sid (Hashtbl.create 5);
  TrackF.compute [(List.hd f.sbody.bstmts)]
;;

let get_alias (e:exp) (id:int) : (alias) =
  match (get_id_state DFM.stmtStartData id) with
      Some table -> (
        try (Hashtbl.find table e)
        with Not_found -> Dead
      )
    | None -> Dead
;;
          

let must_alias (e1:exp) (e2:exp) (id:int) : (bool) =
  try 
    match (get_alias e1 id) with
        Next e when (Util.equals e2 e) -> true
      | _ -> false
  with Not_found -> false
;;
          




  
