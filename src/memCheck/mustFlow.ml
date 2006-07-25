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

type must_table = (varinfo, varPtr) Hashtbl.t;;

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
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "{%s}" ( "Print not implemented..."
    );;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: t) = state;;

  
  (* Merge points take the intersection of the two sets *)
  let combinePredecessors (s: stmt) ~(old_state: t) (new_state: t) =

    (* Print incoming state *)
    if (!dbg_must_combine) then (
      ignore (printf "MUST COMBINE: Examining State: %d:\n" s.sid);
      ignore (printf "MUST COMBINE: Incoming old_state:\n");
      print_must_table old_state;
      ignore (printf "MUST COMBINE: Incoming merge:\n");
      print_must_table new_state;
      flush stdout;
    );

    (* Create a NEW state by merging the two old state.  Be careful since OCaml
     * defaults to using references to copy a hash table...
     *)
    let state = Hashtbl.create 5 in
      
      (* For each element in old_state state, if it has the same alias good,
       * else drop it *)
      Hashtbl.iter 
        (fun v a -> 
            try 
              if (Util.equals (Hashtbl.find new_state v) a) then
                Hashtbl.add state v a
              else
                Hashtbl.add state v Dead
            with _ -> Hashtbl.add state v Dead)
        old_state;
     
      (* Print outgoing state *)
      if (!dbg_must_combine) then (
        ignore (printf "MUST COMBINE: Post merge:\n");
        print_must_table state;
      );

      (* Check to see if the state is the same INDEPENDENT of the order of
       * entries by checking that:
       *   all enteries is the old state are in the generated state AND
       *   all enteries in the generated state are in the old state
       * 
       * Convince yourself that we do not need to check the new_state :-)
       *)
      if (
        try
          let seo =
            (Hashtbl.fold
               (fun e a b -> b && (Util.equals (Hashtbl.find old_state e) a))
               state
               true)
          in
          let soe = 
            (Hashtbl.fold
               (fun e a b -> b && (Util.equals (Hashtbl.find state e) a))
               old_state
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
        
        Set ((Var v, _), CastE (_, Const (CInt64 (z, _, _))), _)
          when ((compare z (Int64.of_int 0)) == 0) ->
          Hashtbl.replace state v Null;
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n maps var %s to Null\n" d_instr i v.vname);
          DF.Done state
      
      | Set ((Var v, _), e, _) ->
          begin
            match e with
                Const c ->
                  Hashtbl.replace state v End;
                  if (!dbg_must_i) then
                    ignore (printf "MUST I: %a\n maps var %s to End\n" d_instr i v.vname);
                  DF.Done state

              | AddrOf (Var v, _) 
              | StartOf (Var v, _) ->
                  Hashtbl.replace state v End;
                  if (!dbg_must_i) then
                    ignore (printf "MUST I: %a\n maps var %s to End\n" d_instr i v.vname);
                  DF.Done state

              | Lval (Var v2, _) ->
                  Hashtbl.replace state v (Next v2);
                  if (!dbg_must_i) then
                    ignore (printf "MUST I: %a\n maps var %s to %s\n" d_instr i v.vname v2.vname);
                  DF.Done state

              (* Is this even correct?  Looks like it may be to general.  The
               * must alais analysis should be remembering offsets, correct...
               * *)
              | BinOp (_, eAlias, Const _, _)
              | BinOp (_, Const _, eAlias, _)
              | UnOp (_, eAlias, _) ->
                  let vop = U.getVarinfoFromExp eAlias in
                    begin
                      match vop with
                          Some v2 ->
                            Hashtbl.replace state v End (*Shane: (Next v2)*);
                            if (!dbg_must_i) then
                              ignore (printf "MUST I: %a\n maps var %s into %s\n" 
                                        d_instr i v.vname v2.vname);
                        | None ->
                            if (!dbg_must_i) then
                              ignore (printf "MUST I: %a\n maps var %s to End (can't e2v %a)\n" 
                                        d_instr i v.vname d_exp eAlias);
                    end;
                    DF.Done state
                    

              (* | UnOp (_, _, _) *)
              | BinOp (_, _, _, _) ->
              
                  Hashtbl.replace state v End;
                  if (!dbg_must_i) then
                    ignore (printf "MUST I: %a\n maps var %s to End\n" d_instr i v.vname);
                  DF.Done state

              | CastE (t, Lval (Var v2, _)) ->
                  Hashtbl.replace state v (Next v2);
                  if (!dbg_must_i) then
                    ignore (printf "MUST I: %a\n maps var %s to %s\n" d_instr i v.vname v2.vname);
                  DF.Done state

                  
              | _ -> 
                  if (!dbg_must_i) then
                    ignore (printf "MUST I: %a\n is ignored\n" d_instr i);
                  DF.Done state
          end

      (* I am not convinced that this is completely correct.  My concern is that
       * the base varinfo in v1 may be moderatly removed from the varinfo that
       * is having v2 put into it.  I will comment this out for now. *)
            (*
      | Set ((Mem e1, _), e2, _) when
          match (typeOf e1) with
              TPtr (TPtr (_, _), _) -> true
            | _ -> false
        ->
          let v1op = U.getVarinfoFromExp e1 in
          let v2op = U.getVarinfoFromExp e2 in
            begin
              match (v1op, v2op) with
                  (Some v1, Some v2) ->
                    Hashtbl.replace state v1 (Next v2);
                    if (!dbg_must_i) then
                      ignore (printf "MUST I: %a\n maps var %s via ptr ptr to %s\n" 
                                d_instr i v1.vname v2.vname);
                | _ ->
                    if (!dbg_must_i) then
                      ignore (printf "MUST I: %a\n is ignored\n" d_instr i);
            end;
            DF.Done state
             *)
            

      (*
       * I also belive that this is incorrect.  Changing the value pointed to by
       * a pointer, should not effect what that pointer must alias.  It simply
       * effects the value of the object pointed to.
       *)

      (*
      | Set ((Mem (Lval (Var v, _)), _), e, _) ->
          Hashtbl.replace state v End;
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n maps var %s to End\n" d_instr i v.vname);
          DF.Done state
       *)

            
      | Set ((Mem _, _), e, _) ->
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n is ignored\n" d_instr i);
          DF.Done state

      | Call (Some (Var v, _), _, _, _) ->
          Hashtbl.replace state v End;
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n maps var %s to End\n" d_instr i v.vname);
          DF.Done state

      | Call (Some _, _, _, _) ->
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n is ignored\n" d_instr i);
          DF.Done state

      | _ -> 
          if (!dbg_must_i) then
            ignore (printf "MUST I: %a\n is ignored\n" d_instr i);
          DF.Done state

  let doGuard _ _ = DF.GDefault


  (* Statements should not effect alias information *) 
  let doStmt (s: stmt) (state: t) = DF.SUse (Hashtbl.copy state)

  (* All blocks go on worklist. *)
  let filterStmt _ = true

end

module TrackF = DF.ForwardsDataFlow(DFM)

let getStmtState (data: mustTable IH.t) (s: stmt): mustTable option =
  try Some (IH.find data s.sid)
  with Not_found -> None 

let getIdState (data: mustTable IH.t) (id: int): mustTable option =
  try Some (IH.find data id)
  with Not_found -> None 

let print_alias (id:int) =
  match (getIdState DFM.stmtStartData id) with
      Some table -> 
        ignore (printf "\n\nState %d:\n" id);
        Hashtbl.iter 
          (fun key value ->
             ignore (printf "%s -> " key.vname);
             match value with
                 Next v -> ignore (printf "%s\n" v.vname)
               | End -> ignore (printf "End\n")
               | Null -> ignore (printf "Null\n")
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

let query_alias (e:exp) (id:int) : (varPtr) =
  match (getIdState DFM.stmtStartData id) with
      Some table -> (
        try (Hashtbl.find table e)
        with Not_found -> End
      )
    | None -> End
;;
          



  
