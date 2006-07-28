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

let nullPtr = CastE (intPtrType, zero);;

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

        Set (lv, e, _) when (Util.equals e nullPtr) ->
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

(* Run the data flow to generate must alias information for a function *)
let generate_must_alias (f:fundec) =
  IH.clear DFM.stmtStartData;
  IH.add DFM.stmtStartData (List.hd f.sbody.bstmts).sid (Hashtbl.create 5);
  TrackF.compute [(List.hd f.sbody.bstmts)]
;;


(* Helper function that returns the alias information for a given statement ID
 *)                      
let get_id_state (data: must_table IH.t) (id: int): must_table option =
  try Some (IH.find data id)
  with Not_found -> None
;;


(* Print the alias information gatherd for a particular statment ID *)                      
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
 

(* Helper function to return the item that an expression must alias *)
let get_aliases (e:exp) (id:int) : (exp option) =
  match (get_id_state DFM.stmtStartData id) with
      Some table -> (
        try match (Hashtbl.find table e) with
            Next e -> Some e
          | Null -> Some nullPtr
          | _ -> None
        with Not_found -> None
      )
    | None -> None
;;
          

(* Helper function to return the items that must alias an expression *)
let get_aliased_by (e:exp) (id:int) : (exp list) =
  match (get_id_state DFM.stmtStartData id) with
      Some table -> (
        try 
          Hashtbl.fold 
            (fun key binding back_aliases -> 
               if (Util.equals binding (Next e)) then 
                 (key :: back_aliases)
               else 
                 back_aliases
            ) 
            table []
        with Not_found -> []
      )
    | None -> []
;;
          

(* For a given expression at a particular statement, return the alias
 * information for that state. Note that this includes items found through a
 * transitive follow of must alias information, but only in the forward
 * direction.
 *)
let get_aliases (e:exp) (id:int) : (exp list) =
  
  let rec get_aliases_helper (old_aliases:exp list) =

    (* Merge new forwards aliases into list *)
    let new_aliases = 
      List.fold_left 
        (fun growing_aliases e -> match (get_aliases e id) with
           | Some e_next when not (List.mem e_next growing_aliases) -> 
               e_next :: growing_aliases
           | _ -> growing_aliases
        )
        old_aliases
        old_aliases
    in

    (* We can stop if the new aliases list is the same as the old alias list.
    * Since new _must_ contain everything in old due to the way this function is
    * written, we simply need to check if new is also a subset of old to test for
    * equality. *)
    let new_subset_of_old = 
      List.for_all (fun e -> List.mem e old_aliases) new_aliases
    in

      if new_subset_of_old then new_aliases
      else (get_aliases_helper new_aliases)
  in

    get_aliases_helper [e]
;;

                      
(* Retrun true if expression e1 must alias expression e2 *)
let must_alias (e1:exp) (e2:exp) (id:int) : (bool) =
  List.mem e2 (get_aliases e1 id)
;;
          




  
