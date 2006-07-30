open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module E = Errormsg
module U = MemUtil

module S = Set

             
(* Runtime debuging flags. *)
let dbg_equiv_i = ref false;;
let dbg_equiv_combine = ref false;;

(* Dataflow specific debugging *)
let dbg_equiv_df = ref false;;

(* Equivalency information will be stored as sets of expressions *)
module Equiv =
struct
  type t = exp
  let compare i j = compare i j
end;;

module EquivSet = Set.Make(Equiv);;

let nullPtr = CastE (intPtrType, zero);;

type equiv_table = EquivSet.t list;;

(* Helper function to print an EquivSet *)
let print_equiv_table (table:equiv_table) =
  List.iter
    (fun (eq : EquivSet.t) -> 
       ignore (printf "Set ->\n");
       EquivSet.iter (fun e -> ignore (printf "  %a\n" d_exp e)) eq
    )
    table
;;


(* Operations for interacting with lists of sets *)
module ListSet = 
struct
  
  (* Given an expression and an equiv set (probably contianing the
   * expression), intersect the equiv set with each element of a list of
   * equiv sets (aka equiv set list or eqsl).  Choose from these sets the
   * non-empty set containing the expression. *)
  let equiv_eqsl_intersection (e : exp) (eq : Equiv.t) (eqsl : EquivSet.t list) : EquivSet.t =
    let intersections = 
      EquivSet.fold  
        (fun eq_new eq_list -> 
           let i = EquivSet.inter eq eq_new in
             if EquivSet.mem e i then i::eq_list
             else eq_list
        )
        eqsl []
    in
      if List.length intersections > 1 then
        E.s (E.error "isEquivalent: combinePredecessors: Incorrect math :-(\n")
      else if (List.length intersection = 1) (List.hd intersections)
      else EquivSet.singleton e
  ;;

  (* Given an expression and a list of equiv sets (aka. set list or sl),
   * return true if the expression is in the sl. *)
  let e_in_sl (e : exp) (eqsl : EquivSet.t list) : bool =
    List.exists (fun eq -> EquivSet.mem e eq) eqsl
  ;;
        
  (* eqsl1 is a "subset" of eqsl2 if each set within eqsl1 is also a set
   * within eqsl2 *)
  let is_subset_of (eqsl1 : EquivSet.t list) (eqsl2 : EquivSet.t) : bool =
    List.for_all 
      (fun eq1 -> 
         List.exists (
           fun eq2 -> EquivSet.equal eq1 eq2) 
           eqsl2) 
      eqsl1
  ;;

  (* Remove element from any sets that it occupies *)
  let remove (e : exp) (eqsl: EquivSet.t list) : (EquivSet.t list) =
    List.fold_left
      (fun result eq -> (EquivSet.remove e)::result)
      [] eqsl
  ;;

  (* Add the pair of elements to the set that contains e1 or the set that
   * contains e2.  Note that if both e1 and e2 are contained in a set then this
   * is a mistake. *)
  let add_pair (e1 : exp) (e2 : exp) (eqsl : EquivSet.t list) : (EquivSet.t list) =
    let e1_set = 
      try Some (List.find (fun eq -> EquivSet.mem e1 eq) eqsl)
      with Not_found -> None
    in
    let e2_set = 
      try Some (List.find (fun eq -> EquivSet.mem e2 eq) eqsl)
      with Not_found -> None
    in
      match (e1_set, e2_set) with
          (None, None) -> (EquivSet.add e1 (EquivSet.singleton e2))::eqsl
        | (Some e1, None) ->
(*TODO: Start here *)
    
  
end;;



module DFM = struct

  (* Vital stats for this dataflow. *)
  let name = "equivFlow";;
  let debug = dbg_equiv_df;;
  type t = equiv_table;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "{%s}" ( 
      print_equiv_table state
    );;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: t) = state;;

  
  (* Merge points take the intersection of the two sets *)
  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) : t option =  

    (* Print incoming state *)
    if (!dbg_must_combine) then (
      ignore (printf "MUST COMBINE: Examining State: %d:\n" s.sid);
      ignore (printf "MUST COMBINE: Incoming old state:\n");
      print_equiv_table old;
      ignore (printf "MUST COMBINE: Incoming merge:\n");
      print_equiv_table new_state;
      flush stdout;
    );

    (* Create a NEW state by merging the two old state.  If the state is
    * different that the old state then we need to do stuff.  *)
    let state =

         (* TODO: This could be correct... *) 
         List.fold_left
           (fun eq l ->
              (EquivSet.fold
                 (fun e eqsl -> 
                    if (ListSet.e_in_sl e eqsl) then eqsl
                    else (ListSet.equiv_eqsl_intersection e eq eqsl)::eqsl
                 ) eq []
              )::l
           )
           [] old
    in

      if (ListSet.is_subset_of state old) && (ListSet.is_subset_of old state) then None
      else (Some state)
  ;;


  (* Go go data flow!
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
          




  
