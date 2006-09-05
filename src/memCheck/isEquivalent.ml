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
let dbg_equiv_get_aliases = ref false;;
let dbg_equiv_get_equiv_set = ref false;;
let dbg_equiv_stmt_summary = ref false;;
let verbose = ref false;;

(* Dataflow specific debugging *)
let dbg_equiv_df = ref false;;

(* Reference to the current statment *)
let currentStmt = ref (mkEmptyStmt ());;

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

  (* Remove element or super element from any sets that it occupies. *)
  let remove (e : exp) (eqsl: EquivSet.t list) : (EquivSet.t list) =
    List.fold_left
      (fun result eq -> 
         (EquivSet.filter (fun e2 -> not (U.is_subexpression_of e e2)) eq)::result)
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


  (* Custom helper function to obtain the aliases of an expression.
   * 
   * An equivalent value is:
   * - Set containing e called s0
   * - Elements in the set containing *x, where x is a member of a set 
   *   containing the an element &y (y is any member of S0)
   * - Elements in the set containing **x, where x is a member of a set 
   *   containing the an element &&y or &z (y is any member of S0 and z is any
   *   member of s1)
   * - And so on...
   *
   * A simple way to accomplish this is to:
   * - Call getEquiv
   * - Sort and uniqify the resulting list and call this l_0
   * - Call getEquiv on each element of l_0
   * - Sort and uniquify the union of these calls and call this l_1
   * - Continue this procedure until l_n = l_(n-1)
   *
   * Note that in contrast to this is an ALIAS of an expression e:
   * - Find the equiv set of &e
   * - Dereference each of these
   *)
  let getEquiv (e:exp) (state:t) : exp list = 
    
    if !dbg_equiv_get_aliases then (
      ignore (printf 
                "IsEquivalent.DFM.getEquiv: Alias search looking at expression %a\n" 
                d_exp e);
      flush stdout;
    );
    
    let rec get_aliases_helper (e:exp) =  
      match (stripCasts e) with
          Lval lv ->
            let aliases = 
              try (EquivSet.elements 
                     (List.find 
                        (fun eq -> EquivSet.mem (mkAddrOf lv) eq) 
                        state
                     )
              )
              with Not_found -> []
            in
              List.map 
                (fun e -> Lval (mkMem ~addr:e ~off:NoOffset)) 
                (aliases @ (get_aliases_helper (mkAddrOf lv)))

        | AddrOf lv
        | StartOf lv ->
            if !dbg_equiv_get_aliases then (
              ignore (printf "Alias search stopping with terminal expression %a\n" d_exp e);
              flush stdout;
            );
            []


        | _ ->
            if !verbose then (
              E.warn "IsEquivalent.DFM.getEquiv:";
              E.warn "  Do not understand expression %a." d_exp e;
              E.warn "  Stopping recursion and returning empty alias set for this term.";
            );
            []
    in


    let sort_and_uniq (el:exp list) : exp list =
      let rec uniq el = match el with
          [] -> []
        | hd::[] -> [hd]
        | hd::next::rest ->
            if (Util.equals hd next) then
              uniq (hd::rest)
            else
              hd::(uniq (next::rest))
      in
      uniq (List.sort compare el)
    in

  
    let get_all_aliases (el:exp list) : exp list = 

      let direct = 
        (List.fold_left
           (fun el e ->
              try (EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) state))
              with Not_found -> [])
           [] el
        )
      in

      let indirect = 
        (List.fold_left
           (fun el e -> (sort_and_uniq (get_aliases_helper e) @ el))
           [] el
        )
      in

        sort_and_uniq (direct @ indirect)
    in
      
    let l0 = ref [] in
    let l1 = ref [] in
 
      l0 := get_all_aliases [e];
      l1 := get_all_aliases !l0;

      while not ((compare !l0 !l1) = 0) do
        if !dbg_equiv_get_aliases then (
          ignore (printf "Aliases of expression %a:\n" d_exp e);
          List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) !l0;
          flush stdout;
        );
        l0 := !l1;
        l1 := (get_all_aliases !l0);
      done;
  
      if !dbg_equiv_get_aliases then (
        ignore (printf "Aliases of expression %a:\n" d_exp e);
        List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) !l0;
        flush stdout;
      );
      !l0
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
     * See getEquiv for more information on what an alias is.
     *)
    let kill (e:exp) (state:t) : t =

      let address_of (e:exp) : exp option =
        match (stripCasts e) with
            Lval lv -> Some (mkAddrOf lv)
        | _ when isIntegralType (typeOf e) -> None
        | _ -> 
            E.warn 
              "IsEquivalent.DFM.doInstr: Unable to make address of non-lval expression %a." 
              d_exp e;
            None
      in


      let aliases =
        List.map  
          (fun e -> Lval (mkMem ~addr:e ~off:NoOffset)) 
          (match (address_of e) with
               Some a -> getEquiv a state
             | _ -> []
          )
      in

        (* Print the aliases *) 
        if (!dbg_equiv_i) then (
          ignore (printf "isEquiv: doInstr: Found aliases of expression %a:\n" d_exp e);
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

      | Set (lv, e, _) -> begin match (stripCasts e) with
            Lval lv2
          | AddrOf lv2
          | StartOf lv2 ->
              let state = kill (Lval lv) state in
              let state = ListSet.add_pair (Lval lv) (stripCasts e) state in
                dbg (Lval lv) (stripCasts e) state;
                DF.Done state

          | _ -> 
              let state = kill (Lval lv) state in
                if !verbose then (
                  E.warn "IsEquivalent.DFM.doInstr:";
                  E.warn "  Do not understand RHS of instrurtion %a." d_instr i;
                  E.warn  "  Skipping.";
                  dbg (Lval lv) (Lval lv) state;
                );
                DF.Done state
        end

      | Call (None, _, formals, _) ->
          let state = List.fold_left (fun s e -> kill e s) state formals in
            if (!dbg_equiv_i) then (
              ignore (printf "isEquiv: doInstr: %a\n" d_instr i);
              print_equiv_table state;
              flush stdout;
            );
            DF.Done state

      | Call (Some lv, _, formals, _) ->
 
          let state = kill (Lval lv) state in
          let state = List.fold_left (fun s e -> kill e s) state formals in
          let state = ListSet.add_singleton (Lval lv) state in
            dbg (Lval lv) (Lval lv) state;
            DF.Done state

      | _ -> 
          E.warn "IsEquivalent.DFM.doInstr: Ignoring instruction %a" d_instr i;
          DF.Done state
  ;;

  
  let doGuard _ _ = DF.GDefault;;


  (* Statements should not effect alias information *) 
  let doStmt (s: stmt) (state: t) = 
    
    currentStmt := s;
    
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

(* Run the data flow to generate must alias information for a function.  Need to
 * jump start the dataflow with information about global variables and formals.
 *)
let generate_equiv (f:fundec) (cilFile:file): unit =

  let global_vars = 
    foldGlobals 
      cilFile
      (fun s g -> match g with
           GVarDecl (v, l) | GVar (v, _, l) -> v::s
         | GFun (fd, l) -> s
         | _ -> s
      ) 
      []
  in

  let start_state = 
    List.fold_left
      (fun start_state v -> ListSet.add_singleton (Lval (var v)) start_state)
      []
      (global_vars @ f.sformals @ f.slocals)
  in

  IH.clear DFM.stmtStartData;
  IH.add DFM.stmtStartData (List.hd f.sbody.bstmts).sid start_state;
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

  let e = stripCasts e in
  
    match (get_id_state DFM.stmtStartData id) with
        Some table -> (
          let direct = 
            try (EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) table))
            with Not_found -> []
          in

          let indirect = (DFM.getEquiv e table)
          in

            if !dbg_equiv_get_equiv_set then (
              ignore (printf "\n");
              ignore (printf "Direct to %a:\n" d_exp (stripCasts e));
              List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) direct;
              ignore (printf "Inirect to %a:\n" d_exp e);
              List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) indirect;
              flush stdout;
            );

            let sort_and_uniq (el:exp list) : exp list =
              let rec uniq el = match el with
                  [] -> []
                | hd::[] -> [hd]
                | hd::next::rest ->
                    if (Util.equals hd next) then
                      uniq (hd::rest)
                    else
                      hd::(uniq (next::rest))
              in
                uniq (List.sort compare el)
            in
              sort_and_uniq (indirect @ direct)

        )
      | None -> 
        []
;;


(* Retrun true if expression e1 must alias expression e2 *)
let is_equiv (e1:exp) (e2:exp) (id:int) : (bool) =
  let e1 = stripCasts e1 in
  let e2 = stripCasts e2 in
    List.mem e2 (get_equiv_set e1 id)
;;






