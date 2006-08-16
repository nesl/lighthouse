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
let dbg_equiv_stmt_summary = ref false;;

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
let print_equiv_table (table : equiv_table) =
  List.iter
    (fun (eq : EquivSet.t) -> 
       ignore (printf "* Set ->\n");
       EquivSet.iter (fun e -> ignore (printf "    %a\n" d_exp e)) eq
    )
    table
;;


(* Operations for interacting with lists of sets *)
module ListSet = struct

  (* Given an expression and an equiv set (probably contianing the
   * expression), intersect the equiv set with each element of a list of
   * equiv sets (aka equiv set list or eqsl).  Choose from these sets the
   * non-empty set containing the expression. *)
  let intersection (e : exp) (eq : EquivSet.t) (eqsl : EquivSet.t list) : EquivSet.t =
    let intersections = 
      List.fold_left  
        (fun eq_list eq_new -> 
           let i = (EquivSet.inter eq eq_new) in
             if (EquivSet.mem e i) then (i::eq_list)
             else eq_list
        )
        [] eqsl
    in
      if List.length intersections > 1 then
        E.s (E.error "isEquivalent: combinePredecessors: Incorrect math :-(\n")
      else if (List.length intersections = 1) then (List.hd intersections)
      else EquivSet.singleton e
  ;;

  (* Given an expression and a list of equiv sets return true if the 
   * expression is in one of the sets in the list. *)
  let e_mem (e : exp) (eqsl : EquivSet.t list) : bool =
    List.exists (fun eq -> EquivSet.mem e eq) eqsl
  ;;

  (* eqsl1 is a "subset" of eqsl2 if each set within eqsl1 is also a subset of a
   * set within eqsl2 *)
  let subset (eqsl1 : EquivSet.t list) (eqsl2 : EquivSet.t list) : bool =
    List.for_all 
      (fun eq1 -> List.exists (fun eq2 -> EquivSet.subset eq1 eq2) eqsl2) 
      eqsl1
  ;;

  (* Remove element from any sets that it occupies *)
  let remove (e : exp) (eqsl: EquivSet.t list) : (EquivSet.t list) =
    List.fold_left
      (fun result eq -> (EquivSet.remove e eq)::result)
      [] eqsl
  ;;


  let add_singleton (e : exp) (eqsl : EquivSet.t list) : (EquivSet.t list) =
    let eq_op = 
      try Some (List.find (fun eq -> EquivSet.mem e eq) eqsl)
      with Not_found -> None
    in
      match eq_op with
          None -> (EquivSet.singleton e)::eqsl
        | Some _ -> E.s (E.error "isEquivalent: ListSet.add_pair: Invalid list set state\n")
  ;;
  
  (* Add the pair of elements to the set that contains e1 or the set that
   * contains e2.  Note that if both e1 and e2 are contained in a set then this
   * is a mistake. *)
  let add_pair (e1 : exp) (e2 : exp) (eqsl : EquivSet.t list) : (EquivSet.t list) =

    let eq1_op = 
      try Some (List.find (fun eq -> EquivSet.mem e1 eq) eqsl)
      with Not_found -> None
    in

    let eq2_op = 
      try Some (List.find (fun eq -> EquivSet.mem e2 eq) eqsl)
      with Not_found -> None
    in

    let update (add_exp : exp) (set : EquivSet.t) (eqsl : EquivSet.t list) : (EquivSet.t list) =
      List.fold_left 
        (fun new_eqsl eq ->
           if (EquivSet.equal set eq) then (EquivSet.add add_exp eq)::new_eqsl
           else eq::new_eqsl
        ) 
        [] eqsl
    in

      match (eq1_op, eq2_op) with
          (None, None) -> 
            (EquivSet.add e1 (EquivSet.singleton e2))::eqsl
        | (Some eq1, None) -> 
            update e2 eq1 eqsl
        | (None, Some eq2) -> 
            update e1 eq2 eqsl
        | (Some _, Some _) -> 
            E.s (E.error "isEquivalent: ListSet.add_pair: Invalid list set state\n")
  ;;

end



(*
 ****************************************
 Data Flow                                        
 ****************************************
 *)

module DFM = struct

  (* Vital stats for this dataflow. *)
  let name = "equivFlow";;
  let debug = dbg_equiv_df;;
  type t = equiv_table;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) = dprintf "{%s}" (
    ignore (print_equiv_table state);
    "Cheating...\n"
  );;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: t) = state;;


  (* Merge points take the intersection of the two sets *)
  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) : t option =  

    (* Print incoming state *)
    if (!dbg_equiv_combine) then (
      ignore (printf "IS_EQUIV COMBINE: Examining State: %d:\n" s.sid);
      ignore (printf "IS_EQUIV COMBINE: Incoming old state:\n");
      print_equiv_table old;
      ignore (printf "IS_EQUIV COMBINE: Incoming merge:\n");
      print_equiv_table new_state;
      flush stdout;
    );

    (* Create a NEW state by merging the two old state.  If the state is
     * different that the old state then continue iterating on the dataflow. *)
    let state =

      (* For each set in the old state... *)
      (* Examine each expression within the set... *)
      (* If that expression is alread in the merged list of sets being generated
       * ..(from this or previous passes) then do nothing and move to the next
       * ..expression... *)
      (* Else, find the intersection of the set containing the expression from
       * ..the old state and each set in the new state and add this
       * ..intersection to the merged output. *)
      List.fold_left
        (fun merged_list eq ->
           (EquivSet.fold
              (fun e tmp_merged_list -> 
                 if ((ListSet.e_mem e merged_list) || 
                     (ListSet.e_mem e tmp_merged_list)) then 
                   tmp_merged_list
                 else 
                   (ListSet.intersection e eq new_state)::tmp_merged_list
              ) eq []
           ) @ merged_list
        )
        [] old
    in

      if (!dbg_equiv_combine) then (
        ignore (printf "IS_EQUIV COMBINE: Outging state:\n");
        print_equiv_table state;
        flush stdout;
      );
      
      if (ListSet.subset state old) && (ListSet.subset old state) then None
      else (Some state)
  ;;


  (* Custom helper function to obtain the aliases of an expression *)
  let getAliases (e:exp) (state:t) : exp list = 
    match e with
        Lval lv
      | CastE (_, Lval lv) ->
          let aliases = 
            try (EquivSet.elements (List.find (fun eq -> EquivSet.mem (mkAddrOf lv) eq) state))
            with Not_found -> []
          in
            List.map (fun e -> Lval (mkMem ~addr:e ~off:NoOffset)) aliases
        
      | _ -> 
          E.warn "mustFlow getAliases:\n";
          E.warn "  Do not understand expression %a.\n" d_exp e;
          E.warn "  Failing to return any aliases.\n";
          []
  ;;
  
  
  (* Go go data flow!
   *)
  let doInstr (i: instr) (state: t): t DF.action =
   
    (* Debugging  helper function *)
    let dbg e1 e2 t =
      if (!dbg_equiv_i) then (
        ignore (printf "isEquiv: doInstr: %a\n  maps exp %a to %a\n" 
                  d_instr i d_exp e1 d_exp e2);
        print_equiv_table t;
        flush stdout;
      )
      else ()
    in

    (* To kill an expression e during the data flow:
     * 
     * - Find aliases {l1, l2, ..., ln} of e
     * - For each li, remove any expressions containing li
     * - Remove e
     *
     * In this context an alias of e is the set containing &e (or &&e or &&&e,
     * etc.).  For example if "&li = &e" then li is an alias of e.
     *
     * TODO: This must recursivly continue looking for aliases.  Currently we
     * find one level of "address of", but this should continue until the
     * address of the most recent expression is no longer an lval.
     *     
     * TODO: Kill function needs to remove any expression CONTAINING a member of
     * the kill set.
     *)

    let kill (e:exp) (state:t) : t =
      let aliases = getAliases e state in
        (* Print the aliases *) 
        if (!dbg_equiv_i) then (
          ignore (printf "Found aliases of expression %a:\n" d_exp e);
          List.iter
            (fun e -> ignore (printf "  %a\n" d_exp e))
            aliases;
          flush stdout;
        );
       
        let state = 
          List.fold_left (fun state e -> ListSet.remove e state) state aliases 
        in
          ListSet.remove e state
    in
    
    match i with

        Set (lv, e, _) when (Util.equals e nullPtr) ->
          let state = kill (Lval lv) state in
          let state = ListSet.add_pair (Lval lv) (nullPtr) state in
            dbg (Lval lv) nullPtr state;
            DF.Done state

      | Set (lv, e, _) when (isConstant e) || (match isInteger e with None -> false | _ -> true) ->
          let state = kill (Lval lv) state in
          let state = ListSet.add_singleton (Lval lv) state in
            dbg (Lval lv) (Lval lv) state;
            DF.Done state

      | Set (lv, e, _) -> begin match e with
            Lval lv2
          | CastE (_, Lval lv2) ->
              let state = kill (Lval lv) state in
              let state = ListSet.add_pair (Lval lv) (Lval lv2) state in
                dbg (Lval lv) (Lval lv2) state;
                DF.Done state

          | _ -> 
              let state = kill (Lval lv) state in
                E.warn "mustFlow doInstr:\n";
                E.warn "  Do not understand RHS of instrurtion %a.\n" d_instr i;
                E.warn  "  Skipping.\n";
                dbg (Lval lv) (Lval lv) state;
                DF.Done state
        end

      | Call (Some lv, _, _, _) ->
          (* TODO: need to kill all formals! *)
          let state = kill (Lval lv) state in
          let state = ListSet.add_singleton (Lval lv) state in
            dbg (Lval lv) (Lval lv) state;
            DF.Done state

      | _ -> 
          (* TODO: Remove this after debugging and replace with version
           * commented out below. *)
          E.warn "mustFlow doInstr: Ignoring instruction %a\n" d_instr i;
          DF.Done state
  ;;

  
  let doGuard _ _ = DF.GDefault;;


  (* Statements should not effect alias information *) 
  let doStmt (s: stmt) (state: t) = 
    if (!dbg_equiv_stmt_summary) then (
      ignore (printf "isEquiv: doInstr: Entering statement %d with state\n" s.sid); 
      print_equiv_table state;
      flush stdout
    );
      DF.SDefault
  ;;


  (* All blocks go on worklist. *)
  let filterStmt _ = true;;

end



module TrackF = DF.ForwardsDataFlow(DFM)

(* Run the data flow to generate must alias information for a function *)
let generate_equiv (f:fundec) =
  IH.clear DFM.stmtStartData;
  IH.add DFM.stmtStartData (List.hd f.sbody.bstmts).sid [];
  TrackF.compute [(List.hd f.sbody.bstmts)]
;;


(* Helper function that returns the equivalince sets for a given statement ID
 *)                      
let get_id_state (data: equiv_table IH.t) (id: int): equiv_table option =
  try Some (IH.find data id)
  with Not_found -> None
;;


(* Print the alias information gatherd for a particular statment ID *)                      
let print_equiv_sets (id:int) =
  match (get_id_state DFM.stmtStartData id) with
      Some table -> 
        ignore (printf "\n\nState %d:\n" id);
        print_equiv_table table
    | None ->
        ignore (printf "\n\nUnable to find state %d\n" id)
;;


(* Helper function to return the items that an expression is equivalent to *)
let get_equiv_set (e:exp) (id:int) : (exp list) =
  match (get_id_state DFM.stmtStartData id) with
      Some table -> (
        let direct = 
          try (EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) table))
          with Not_found -> []
        in

        let indirect = 
            List.fold_left 
              (fun equiv e ->
                equiv @ ( 
                  try (EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) table))
                  with Not_found -> []
                )
              )
              []
              (DFM.getAliases e table)
        in

          (*
          ignore (printf "\n");
          ignore (printf "Direct to %a:\n" d_exp e);
          List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) direct;
          ignore (printf "Inirect to %a:\n" d_exp e);
          List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) indirect;
          *)
          
          indirect @ direct
      
      )
    | None -> []
;;


(* Retrun true if expression e1 must alias expression e2 *)
let is_equiv (e1:exp) (e2:exp) (id:int) : (bool) =
  List.mem e2 (get_equiv_set e1 id)
;;





