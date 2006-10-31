open Cil
open Pretty

module IH = Inthash
module DF = Dataflow
module E = Errormsg
module U = MemUtil

module S = Set


(* Runtime debuging flags. *)
let dbg_is_equiv_i = ref false;;
let dbg_is_equiv_c = ref false;;
let dbg_is_equiv_get_aliases = ref false;;
let dbg_is_equiv_get_equiv_set = ref false;;
let dbg_is_equiv_stmt_summary = ref false;;
let verbose = ref false;;

(* Dataflow specific debugging *)
let dbg_is_equiv_df = ref false;;

(* Reference to the current statment *)
let currentStmt = ref (mkEmptyStmt ());;

(* List of functions that can allocated and free data *)
let alloc_funcs = ref [("malloc", 0)];;
let free_funcs = ref [("free", 1)];; 

(* Counter used to create temporary names for heap allocated data *)
let heap_counter = ref 0;;

(* Expression representing the null pointer *)
let nullPtr = CastE (intPtrType, zero);;


(****************************************)
(* Collection of modules used to create "equivalency" sets for expressions *)
(****************************************)

(* Equivalency information will be stored as sets of expressions *)
module Equiv =
struct
  type t = exp
  let compare i j = compare i j
end;;

module EquivSet = Set.Make(Equiv);;

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

  (* Remove element or child element from all sets. *)
  let remove (parent : exp) (eqsl: EquivSet.t list) : (EquivSet.t list) =
    List.fold_left
      (fun result eq ->
         let new_set = 
           EquivSet.filter 
             (fun child -> not (U.is_parent_of parent child)) 
             eq 
         in
           if EquivSet.is_empty new_set then 
             result
           else
             new_set::result
      )
      [] eqsl
  ;;


  let add_singleton (e : exp) (eqsl : EquivSet.t list) : (EquivSet.t list) =
    let eq_op = 
      try Some (List.find (fun eq -> EquivSet.mem e eq) eqsl)
      with Not_found -> None
    in
      match eq_op with
          None -> (EquivSet.singleton e)::eqsl
        | Some _ -> 
            ignore (printf "Expression %a is already in table:\n" d_exp e);
            print_equiv_table eqsl;
            flush stdout;
            E.s (E.error "isEquivalent: ListSet.add_pair: Invalid list set state\n")
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


let set_intersect (s1:equiv_table) (s2:equiv_table): equiv_table =
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
             else (
               let new_set = ListSet.intersection e eq s2 in
                 if EquivSet.is_empty new_set then tmp_merged_list
                 else new_set::tmp_merged_list
             )
          ) eq []
       ) @ merged_list
    )
    [] s1
;;


(*
 ****************************************
 Data Flow                                        
 ****************************************
 *)

module DFM = struct

  (* Vital stats for this dataflow. *)
  let name = "equivFlow";;
  let debug = dbg_is_equiv_df;;
  type t = equiv_table;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) = dprintf "{%s}" (
    ignore (print_equiv_table state);
    ""
  );;


  (* Statments do not have a default state *)
  let computeFirstPredecessor (s: stmt) (state: t) = state;;


  (* Merge points take the intersection of the two sets *)
  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) : t option =  

    (* Print incoming state *)
    if (!dbg_is_equiv_c) then (
      ignore (printf "IS_EQUIV COMBINE: Examining State: %d:\n" s.sid);
      ignore (printf "IS_EQUIV COMBINE: Incoming old state:\n");
      print_equiv_table old;
      ignore (printf "IS_EQUIV COMBINE: Incoming merge:\n");
      print_equiv_table new_state;
      flush stdout;
    );

    
    (* Create a NEW state by merging the two old state.  If the state is
     * different that the old state then continue iterating on the dataflow. *)
    let state = set_intersect old new_state in

      if (!dbg_is_equiv_c) then (
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
   * - Call GETeQUIv
   * - Sort and uniqify the resulting list and call this l_0
   * - Call getEquiv on each element of l_0
   * - Sort and uniquify the union of these calls and call this l_1
   * - Continue this procedure until l_n = l_(n-1)
   *
   * Note that an ALIAS of an expression e:
   * - Find the equiv set of &e
   * - Dereference each of these
   *)
  let getEquiv (e:exp) (state:t) : exp list = 

    if !dbg_is_equiv_get_aliases then (
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
            if !dbg_is_equiv_get_aliases then (
              ignore (printf "Alias search stopping with terminal expression %a\n" d_exp e);
              flush stdout;
            );
            []

        (* Blah.  Okay, so this is a nice conservative thing to do.  But, field
         * references are being expanded into the form:    
         *     tmp = (unsigned int)(&store) + 4; 
         *     (*(int **)tmp) = (int* ) ker_malloc(42, 1);
         * and requires that we realize that __cil_tmp4 + 4U is really a field
         * of whatever __cil_tmp4 is pointing to.  ASSUMING that we don't have
         * bad pointer math lying around, this should be okay to do.
         *)
                                                       
        | BinOp (PlusPI, e1, _, _)
        | BinOp (IndexPI, e1, _, _)
        | BinOp (MinusPI, e1, _, _) ->
            if !verbose then (
              E.warn "IsEquivalent.DFM.getEquiv:";
              E.warn "  Using entire pointer arith expression %a." d_exp e;
            );
            [e]
        
        | BinOp (PlusA, e1, c, _)
        | BinOp (MinusA, e1, c, _) when isConstant c ->
            if !verbose then (
              E.warn "IsEquivalent.DFM.getEquiv:";
              E.warn "  Using entire pointer arith expression %a." d_exp e;
            );
            [e]
       
        | BinOp (PlusA, c, e1, _)
        | BinOp (MinusA, c, e1, _) when isConstant c ->
            if !verbose then (
              E.warn "IsEquivalent.DFM.getEquiv:";
              E.warn "  Using entire pointer arith expression %a." d_exp e;
            );
            [e]

        | _ ->
            if !verbose then (
              E.warn "IsEquivalent.DFM.getEquiv:";
              E.warn "  Do not understand expression %a." d_exp e;
              E.warn "  Stopping recursion and returning empty alias set for this term.";
            );
            []
    in


    let checked = ref [] in
    
    let get_all_aliases (el:exp list) : exp list = 
      
      (* Filter out expression that we have already checked. *)
      let el = 
        List.filter (fun e -> not (List.mem e !checked)) el 
      in

      (* For each element e of el, grab the set that contains e.  Combine all of
       * these into one big set. *)
      let direct = 
        (List.fold_left
           (fun dl e ->
              try ((EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) state)) @ dl)
              with Not_found -> [])
           [] el
        )
      in

      
      (* *)
      let indirect = 
        (List.fold_left
           (fun il e -> (U.sort_and_uniq (get_aliases_helper e) @ il))
           [] el
        )
      in

        (* Update the checked list *)
        checked := (!checked @ el);
     
        (* 
        ignore (printf "+ Direct aliases:\n");
        List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) direct;
        ignore (printf "- Indirect aliases:\n");
        List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) indirect;
        ignore (printf "* Checked:\n");
        List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) !checked;
        flush stdout;
         *)
      
        U.sort_and_uniq (direct @ indirect)
    in

    let l0 = ref [] in
    let l1 = ref [] in
      
      l0 := get_all_aliases [e];
      l1 := get_all_aliases !l0;

      while not ((compare !l0 !l1) = 0) do
        checked := []; 
        if !dbg_is_equiv_get_aliases then (
          ignore (printf "Aliases of expression %a:\n" d_exp e);
          List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) !l0;
          flush stdout;
        );
        l0 := !l1;
        l1 := (get_all_aliases !l0);
      done;

      if !dbg_is_equiv_get_aliases then (
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
      if (!dbg_is_equiv_i) then (
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


      let aliases_of (e:exp) : exp list =
        List.map  
          (fun e -> Lval (mkMem ~addr:e ~off:NoOffset)) 
          (match (address_of e) with
               Some a -> getEquiv a state
             | _ -> []
          )
      in


      let kill_set = aliases_of e in

      let kill_set = U.sort_and_uniq kill_set in

        (* Print the aliases and parent *) 
        if (!dbg_is_equiv_i) then (
          ignore (printf "isEquiv: doInstr: Found killset for expression %a:\n" d_exp e);
          List.iter
            (fun e -> ignore (printf "  %a\n" d_exp e))
            kill_set;
          flush stdout;
        );

        let state = 
          List.fold_left (fun state e -> ListSet.remove e state) state kill_set
        in
          ListSet.remove e state
    in

      match i with

          Set (lv, e, _) when (isZero e) ->
            let state = kill (Lval lv) state in
            let state = ListSet.add_pair (Lval lv) (nullPtr) state in
              dbg (Lval lv) nullPtr state;
              DF.Done state

        | Set (lv, e, _) -> 
            let state = kill (Lval lv) state in
            
              begin match (stripCasts e) with
                  Lval lv2
                | AddrOf lv2
                | StartOf lv2 ->
                    let state = ListSet.add_pair (Lval lv) (stripCasts e) state in
                      dbg (Lval lv) (stripCasts e) state;
                      DF.Done state

                | BinOp (PlusPI, e1, _, _)
                | BinOp (IndexPI, e1, _, _)
                | BinOp (MinusPI, e1, _, _) ->
                    let state = ListSet.add_pair (Lval lv) (stripCasts e) state in
                      dbg (Lval lv) (stripCasts e) state;
                      DF.Done state

                | BinOp (PlusA, e1, c, _)
                | BinOp (MinusA, e1, c, _) when isConstant c ->
                    let state = ListSet.add_pair (Lval lv) (stripCasts e) state in
                      dbg (Lval lv) (stripCasts e) state;
                      DF.Done state


                | BinOp (PlusA, c, e1, _)
                | BinOp (MinusA, c, e1, _) when isConstant c ->
                    let state = ListSet.add_pair (Lval lv) (stripCasts e) state in
                      dbg (Lval lv) (stripCasts e) state;
                      DF.Done state

                | _ -> 
                    let state = ListSet.add_singleton (Lval lv) state in
                      if !verbose then (
                        E.warn "IsEquivalent.DFM.doInstr:";
                        E.warn "  Do not understand RHS of instrurtion %a." d_instr i;
                        E.warn  "  Skipping.";
                        dbg (Lval lv) (Lval lv) state;
                      );
                      DF.Done state
              end

        | Call (rop, e, formals, _) ->
            
            (* First remove references to freed heap data *)
            let (free_names, free_nums) = List.split !free_funcs in
            let state = match e with
                Lval (Var v, NoOffset) ->
                  List.fold_left2 
                    (fun state free_name free_num ->
                       if (v.vname = free_name) then (
                         if free_num = 0 then (
                           E.s (E.error "isEquivalent: doInstr: Cannot free a return value\n")
                         ) else (

                           let e = List.nth formals (free_num - 1) in

                           let direct = 
                             try (EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) state))
                             with Not_found -> []
                           in

                           let indirect = (getEquiv e state)
                           in

                           let equivs = U.sort_and_uniq (indirect @ direct) in

                             List.fold_left
                               (fun state e -> match (stripCasts e) with
                                    Lval (Var v, NoOffset) when 
                                      (Str.string_match (Str.regexp "__heap") v.vname 0) ->
                                      kill e state
                                  | _ -> state
                               )
                               state
                               equivs
                         )
                       ) else (state)
                    )
                    state
                    free_names
                    free_nums

              | _ -> state 
            in

            (* Then kill all formals and the (optional) return lval *)
            let state = 
              List.fold_left 
                (fun s e -> ListSet.add_singleton e (kill e s)) 
                state 
                formals 
            in

            let state = match rop with
                Some lv -> ListSet.add_singleton (Lval lv) (kill (Lval lv) state)
              | None -> state
            in

            (* Finally, add in freshly created heap data *)
            let (alloc_names, alloc_nums) = List.split !alloc_funcs in
            let state = match e with
                Lval (Var v, NoOffset) ->
                  List.fold_left2 
                    (fun state alloc_name alloc_num ->
                       if (v.vname = alloc_name) then (

                         (* Make the heap expression *)
                         let base_name = "__heap_" ^ (string_of_int !heap_counter) in
                         let extended_name = 
                           base_name ^ "_line_" ^ (string_of_int !currentLoc.line) in
                         let heap = makeVarinfo false extended_name voidPtrType in
                           heap_counter := !heap_counter + 5;

                           (* Update state *)
                           if alloc_num = 0 then (
                             match rop with
                                 None -> 
                                   ListSet.add_singleton (Lval (var heap)) state
                               | Some lv ->  
                                   ListSet.add_pair (Lval lv) (Lval (var heap)) state
                           ) else (
                             ListSet.add_pair 
                               (List.nth formals (alloc_num - 1)) (Lval (var heap)) state
                           )
                       ) else (state)
                    )
                    state
                    alloc_names
                    alloc_nums

              | _ -> state 
            in

              if (!dbg_is_equiv_i) then (
                ignore (printf "isEquiv: doInstr: %a\n" d_instr i);
                print_equiv_table state;
                flush stdout;
              );
              DF.Done state

        | _ ->
            if !verbose then (
              E.warn "IsEquivalent.DFM.doInstr: Ignoring instruction %a" d_instr i;
            );
            DF.Done state
  ;;


  let doGuard _ _ = DF.GDefault;;


  (* Statements should not effect alias information *) 
  let doStmt (s: stmt) (state: t) = 

    currentStmt := s;

    if (!dbg_is_equiv_stmt_summary) then (
      ignore (printf "isEquiv: doInstr: Entering statement %d (%a) with state\n" 
                s.sid d_loc (get_stmtLoc s.skind)); 
      print_equiv_table (U.sort_and_uniq state);
      flush stdout
    );
    DF.SDefault
  ;;


  (* All blocks go on worklist. *)
  let filterStmt _ = true;;

end



module TrackF = DF.ForwardsDataFlow(DFM);;

(* Run the data flow to generate must alias information for a function.  Need to
 * jump start the dataflow with information about global variables and formals.
 *)
let generate_equiv (f:fundec) (cilFile:file): unit =

  (* TODO: I am guessing that the crashes from sched.c and sos_info.c are coming
   * from here. *)
  let global_vars = 
    foldGlobals 
      cilFile
      (fun s g -> match g with
         | GFun (fd, _) -> s
         | GVar (v, _, _)
         | GVarDecl (v, _) -> v::s
         | _ -> s
      ) 
      []
  in

    alloc_funcs := [("malloc", 0)];
    free_funcs := [("free", 1)];

    alloc_funcs := !alloc_funcs @ (U.get_alloc_funcs cilFile);
    free_funcs := !free_funcs @ (U.get_free_funcs cilFile);

    let start_state = 
      List.fold_left
        (fun start_state v -> ListSet.add_singleton (Lval (var v)) start_state)
        []
        (global_vars @ f.slocals)
    in

    let start_state = 
      List.fold_left
        (fun start_state v -> 
           if (hasAttribute "sos_release" v.vattr) then (
             let heap = 
               makeVarinfo false ("__heap_" ^ (string_of_int !heap_counter)) voidPtrType
             in
               heap_counter := !heap_counter + 5;
               ListSet.add_pair (Lval (var v)) (Lval (var heap)) start_state
           ) else (
             ListSet.add_singleton (Lval (var v)) start_state
           )
        )
        start_state
        f.sformals
    in

      IH.clear DFM.stmtStartData;
      IH.add DFM.stmtStartData (List.hd f.sbody.bstmts).sid start_state;
      TrackF.compute [(List.hd f.sbody.bstmts)]
;;


(* Helper function that returns the equivalince sets for a given statement ID
 *)                      
let get_equiv_sets (id: int): (exp list list) =
  try List.map (fun s -> EquivSet.elements s) (IH.find DFM.stmtStartData id)
  with Not_found -> []
;;


(* Print the alias information gatherd for a particular statment ID *)                      
let print_equiv_sets (id:int) =
  match (get_equiv_sets id) with
      [] -> 
        ignore (printf "\n\nUnable to find state %d\n" id)
    | table -> 
        ignore (printf "\n\nState %d:\n" id);
        List.iter
          (fun el  -> 
             ignore (printf "* Set ->\n");
             List.iter (fun e -> ignore (printf "    %a\n" d_exp e)) el
          )
          table
;;

(* Helper function *)
let get_equiv_set_helper (e:exp) (state:equiv_table) : (exp list) =
  
  let e = stripCasts e in

  let direct = 
    try (EquivSet.elements (List.find (fun eq -> EquivSet.mem e eq) state))
    with Not_found -> [e]
  in

  let indirect = (DFM.getEquiv e state)
  in

    if !dbg_is_equiv_get_equiv_set then (
      ignore (printf "Directly equivalent to %a:\n" d_exp (stripCasts e));
      List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) direct;
      ignore (printf "Inirectly equivalent to %a:\n" d_exp e);
      List.iter (fun e -> ignore (printf "  %a\n" d_exp e)) indirect;
      flush stdout;
    );
    
    U.sort_and_uniq (indirect @ direct)
;;


(* Helper function to return the items that an expression is equivalent to *)
let get_equiv_set_end (e:exp) (id:int) : (exp list) =

  let state = 
    try (IH.find DFM.stmtStartData id)
    with Not_found -> []
  in

    if !dbg_is_equiv_get_equiv_set then (
      ignore (printf "Equivalent to %a at the end of the statement:\n" d_exp (stripCasts e));
    );
    get_equiv_set_helper e state
      
;;


(* Get equivalent expressions *)
let get_equiv_set_start (e:exp) (s:stmt) : (exp list) =

  (* Make a list of all the prior states *)
  let prior_end_states = 
    List.fold_left
      (fun grow state ->
         (try (IH.find DFM.stmtStartData s.sid) with Not_found -> []) :: grow
      )
      []
      s.preds 
  in
    
    
  (* TODO: START HERE *)
  (* I am guessing that I am improperly doing this merge and ending up
   * with a more or less empty start set.  Test via:
   *
   * ./memory ../unitMemCheck/isStoredUnit07.c --dbg_is_store_f
   * --dbg_is_equiv_get_equiv_set --dbg_is_equiv_stmt_summary
   *
   * *)
  
  let state = 
    try
      List.fold_left 
        (fun grow s ->
           ignore (printf "Folding:\n");
           print_equiv_table s;
           ignore (printf "into:\n");
           print_equiv_table grow;
           flush stdout;
           set_intersect grow s) 
        (List.hd prior_end_states) 
        prior_end_states 
    with Failure _ -> []
  in
  
    ignore (printf "Ending with:\n");
    print_equiv_table state;
    flush stdout;

    if !dbg_is_equiv_get_equiv_set then (
      ignore (printf "Equivalent to %a at the start of the statement:\n" d_exp (stripCasts e));
    );
    get_equiv_set_helper e state
;;


(* Retrun true if expression e1 must alias expression e2 *)
(* TODO: The stripping of type casts is going to require a bit more thought *)
let is_equiv_end (e1:exp) (e2:exp) (id:int) : (bool) =

  let e1 = 
    if (isZero e1) then e1
    else stripCasts e1 
  in

  let e2 = 
    if (isZero e2) then e2
    else stripCasts e2 
  in
    
  let unify_type (e: exp) : exp = match e with   
      StartOf lv -> 
        let e_new = Lval (mkMem (mkAddrOf lv) NoOffset) in
          e_new
    | _ -> e
  in

  let results = List.map unify_type (get_equiv_set_end e1 id) in
  let query = unify_type e2 in

    List.mem query results
;;


let is_equiv_start (e1:exp) (e2:exp) (s:stmt) : (bool) =

  let e1 = 
    if (isZero e1) then e1
    else stripCasts e1 
  in

  let e2 = 
    if (isZero e2) then e2
    else stripCasts e2 
  in
    
  let unify_type (e: exp) : exp = match e with   
      StartOf lv -> 
        let e_new = Lval (mkMem (mkAddrOf lv) NoOffset) in
          e_new
    | _ -> e
  in

  let results = List.map unify_type (get_equiv_set_start e1 s) in
  let query = unify_type e2 in

    List.mem query results

;;



