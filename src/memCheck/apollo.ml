open Cil;;
open Pretty;;
open State;;

module IH = Inthash;;
module DF = Dataflow;;

module E = Errormsg;;
module IE = IsEquivalent;;
module MA = MayAliasWrapper;;

(* Runtime debugging *)
let dbg_apollo_s = ref false;;
let dbg_apollo_i = ref false;;
let dbg_apollo_c = ref false;;
let dbg_apollo_df = ref false;;
let dbg_apollo_g = ref false;;

(* Reference to the current statment *)
let current_stmt = ref (mkEmptyStmt ());;

(* Reference to an error log *)
let error_log = ref "Error Log:\n";;

(****************************************)
(****************************************)
(* Data  flow implementation *)
(****************************************)
(****************************************)
module Apollo_Dataflow = struct

  (* Vital stats for this dataflow. *)
  let name = "apollo";;
  let debug = dbg_apollo_df;;
  type t = dataflow_state;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "%s" (
      let (stores, heaps) = state in
      let s = 
        List.fold_left 
          (fun s store -> 
             let (type_name, e) = match store with
                 (Empty, e) -> ("Empty", e)
               | (Full, e) -> ("Full", e)
               | (Nonheap, e) -> ("Nonheap", e)
               | (Unknown, e) -> ("Unknown", e)
               | (Error, e) -> ("Error", e)
             in
               s ^ (sprint 70 (dprintf "Store %a in state %s\n" d_exp e type_name))
          ) 
          "Stores:\n" 
          stores 
      in
      let s =
        List.fold_left 
          (fun s heap -> 
             s ^ (sprint 70 (dprintf "Heap referenced by expression %a\n" d_exp heap))
          ) 
          (s ^ "Heap Data:\n") 
          heaps 
      in
        s
    )
  ;;


  let computeFirstPredecessor (s: stmt) (state: t): t = state;;


  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) = None;;


  let doInstr (i: instr) (state: t): t DF.action = 
   
    (* 
    ignore (printf "Checking instruction at %a\n" d_loc (get_instrLoc i));
    flush stdout;
    *)

    match i with

        Set (lv, e, _) ->
          begin
            match get_state_from_exp e state !current_stmt with

                Heap e when (is_store (Lval lv) state !current_stmt) ->
                  (* "Transfer" heap data into the store. *)
                  let new_state = fill_store (Lval lv) state !current_stmt in
                  let new_state = remove_heap e new_state !current_stmt in
                    DF.Done new_state

              | Heap e when (is_heap (Lval lv) state!current_stmt ) ->
                  (* Over writing a heap with a reference to another heap *)
                  let new_state = overwrite_heap (Lval lv) state !current_stmt in
                  let new_state = remove_heap e new_state !current_stmt in
                    DF.Done new_state


              | Heap e ->
                  (* Using heap data.  Tranfer depends on use_heap function. *)
                  let new_state = use_heap e state !current_stmt in
                    DF.Done new_state

              | Store e when (is_store (Lval lv) state !current_stmt) ->
                  (* "Transfer" heap data from one store to another. *)
                  let new_state = fill_store (Lval lv) state !current_stmt in
                  let new_state = empty_store e new_state !current_stmt in
                    DF.Done new_state

              | Store e when (is_heap (Lval lv) state !current_stmt) ->
                  (* Over writing a heap with a reference to another heap *)
                  let new_state = overwrite_heap (Lval lv) state !current_stmt in
                  let new_state = use_store e new_state !current_stmt in
                    DF.Done new_state

              | Store e ->
                  (* Using store data.  Transfer depends on use_store function. *)
                  let new_state = use_store e state !current_stmt in
                    DF.Done new_state

              | Complex e ->
                  (* This is something that we do not currently handel. *)
                  E.s (E.bug "%s %s %a\n" 
                         "Apollo.Apollo_Dataflow.doInstr:" 
                         "Unable to update state given set with rval" 
                         d_exp e)

              | Other when (is_store (Lval lv) state !current_stmt) ->
                  (* Over writing a store with some random value *)
                  let new_state = abuse_store e state !current_stmt in
                    DF.Done new_state


              | Other when (is_heap (Lval lv) state !current_stmt) ->
                  (* Over writing a heap with some random value *)
                  let new_state = overwrite_heap e state !current_stmt in
                    DF.Done new_state


              | Other ->
                  (* This set expression has no effect (and is not effected by)
                   * the current state *)
                  DF.Default
          end


      | Call (lvop, Lval((Var v), NoOffset), el, _) ->

          let alloced = MemUtil.get_claim_formals i in
          let freed = MemUtil.get_released i in

          (* Check preconditions *)
(*
          let _ = 

            let (stores, heaps) = state in
            let (pre_full, pre_empty, pre_heap) = 
              SpecParse.lookup_pre !specification v.vname 
            in

              (* Besure that all stores assumed to be full are full *)
              List.iter
                (fun s -> 
                   if not (
                     List.exists 
                       (fun blah -> match blah with
                            (Full, Lval (Var v, _)) when v.vname = s -> true
                          | (_, Lval (Var v, _)) -> false
                          | (_, e) -> E.s (E.bug "Not made to handle expression %a" d_exp e)
                       ) stores
                   ) then
                     ignore (E.error "Bad precondition");
                   ()
                )
                pre_full
              
          in
 *)
          (* Insure that no expression is being released and allocated *)
          let _ = 
            if List.exists 
                 (fun free_e -> 
                    List.exists 
                      (fun alloc_e -> IE.is_equiv_start free_e alloc_e !current_stmt) 
                      alloced
                 ) 
                 freed
            then
              E.s (E.error "%s %s %a"
                     "Apollo.Apollo_Dataflow.doInstr:"
                     "Attempting to free and / or allocate equivalent expressions: %a"
                     d_instr i)
          in

          let new_state = state in

          (* Warn if an empty store is being used as a regular parameter
           * without the sos_claim attribute.  This includes cases where the
           * expression is simply being used, and cases where the expression is
           * going to be freed. *)
          let new_state =
            List.fold_left
              (fun s e -> 
                 if not (List.exists 
                           (fun alloc -> IE.is_equiv_start e alloc !current_stmt) 
                           alloced) &&
                    (is_store e state !current_stmt) 
                 then use_store e s !current_stmt
                 else s
              )
              new_state
              el
          in


          (* Update state with newly allocated data *)
          let new_state =  
            List.fold_left 
              (fun s e ->  
                 if (is_store e s !current_stmt) then fill_store e s !current_stmt
                 else add_heap e s !current_stmt
              ) 
              new_state
              alloced
          in


          (* Updated state with freshly released data *)
          let new_state =
            List.fold_left
              (fun s e ->
                 if (is_store e s !current_stmt) then empty_store e s !current_stmt
                 else remove_heap e s !current_stmt
              )
              new_state
              freed
          in


          (* Update state if function returns allocated data *)
          let new_state = 
            match lvop with
                Some lv when (MemUtil.returns_alloc i) &&
                             (is_store (Lval lv) new_state) !current_stmt ->
                  fill_store (Lval lv) new_state !current_stmt
              | Some lv when (MemUtil.returns_alloc i) ->
                  add_heap (Lval lv) new_state !current_stmt
              | None when (MemUtil.returns_alloc i) ->
                  ignore (E.error "%s (%a): %a"
                            "Return value of allocated data is not being stored at "
                            d_loc (get_stmtLoc !current_stmt.skind)
                            d_instr i);
                  new_state
              | Some lv when (is_store (Lval lv) new_state) !current_stmt ->
                  abuse_store (Lval lv) state !current_stmt
              | Some lv when (is_heap (Lval lv) new_state) !current_stmt ->
                  overwrite_heap (Lval lv) state !current_stmt
              | Some lv -> new_state
              | None -> new_state 
          in

            DF.Done new_state
      
      | Call _ ->
          E.s (E.bug "Have not implemented this form of call")

      | _ -> DF.Default
  ;;


  let doStmt (s: stmt) (state: t) =
    current_stmt := s;
    (*
    ignore (Pretty.printf "%a\n" d_loc (get_stmtLoc !current_stmt.skind));
    ignore (Pretty.printf "%a\n" pretty state);
    flush stdout;
     *)
    DF.SDefault
  ;;

  
  (* Special cases for when an if statment is checking to see if the target
   * expression is Null.  An example of this is checking to see if a call to
   * malloc fails.  When the expression is Null, it can be considered to have
   * been claimed. *)
  let doGuard (e: exp) (state: t) = 
    
    (* Helper function used to determine if a binary comparison is comparing the
     * target expression to a null pointer *)
    let is_target_and_null (e1: exp) (e2: exp) (s: stmt) : bool =
      let is_target = 
        (is_heap e1 state !current_stmt) || 
        (is_heap e2 state !current_stmt)
      in
      let is_null = 
        (IE.is_equiv_start e1 IE.nullPtr !current_stmt) || 
        (IE.is_equiv_start e2 IE.nullPtr !current_stmt)
      in
        if !dbg_apollo_g then (
          ignore (printf "Checking (target, NULL) on %a and %a: (%b, %b)\n"
                    d_exp e1 d_exp e2 is_target is_null);
        );
        is_target && is_null
    in
      
    let transition = match e with

      | Lval lv 
          when (is_heap (Lval lv) state !current_stmt) ->
          (* Unary check to see if item is NOT null.  Continue on with the
           * default action to ensure that the target is stored. *)
          DF.GDefault

      | UnOp (LNot, (Lval lv), _) 
          when (is_heap (Lval lv) state !current_stmt) ->
          (* Unary check to see if item is Null.  Since we know that the target is
           * Null we can abort the check for this branch and directly insert the Null
           * state. *)
          DF.GUse (remove_heap (Lval lv) state !current_stmt)

      | UnOp (LNot, (UnOp (LNot, (Lval lv), _)), _)
          when (is_heap (Lval lv) state !current_stmt) ->
          (* Unary check to see if item is NOT Null.  This form results from the
           * elseGuard clause within the dataflow engine. Continue with default
           * action. *)
          DF.GDefault

      | BinOp (Ne, e1, e2, _) 
          when (is_target_and_null e1 e2 !current_stmt) ->
          (* Binary check to see if item is NOT null *)
          DF.GDefault

      | BinOp (Eq, e1, e2, _)
          when (is_target_and_null e1 e2 !current_stmt) ->
          (* Binary check to see if item is Null *)
          if (is_heap e1 state !current_stmt) then
            DF.GUse (remove_heap e1 state !current_stmt)
          else
            DF.GUse (remove_heap e2 state !current_stmt)
         
      | UnOp (LNot, (BinOp (Ne, e1, e2, _)), _)
          when (is_target_and_null e1 e2 !current_stmt) ->
          (* Binary check to see if item is NOT NOT null.  This form results
           * from the elseGuard clause within the dataflow engine. *)
          if (is_heap e1 state !current_stmt) then
            DF.GUse (remove_heap e1 state !current_stmt)
          else
            DF.GUse (remove_heap e2 state !current_stmt)

      | UnOp (LNot, (BinOp (Eq, e1, e2, _)), _)
          when (is_target_and_null e1 e2 !current_stmt) ->
          (* Binary check to see if item is NOT Null.  This form results 
           * from the elseGuard clause within the dataflow engine. *)
          DF.GDefault

      | _ -> 
          DF.GDefault
    in

      if !dbg_apollo_g then (
        ignore (printf "Apollo.doGuard: Expression %a guarding statement %d (%a)\n" 
                  d_exp e !current_stmt.sid d_loc (get_stmtLoc !current_stmt.skind));
        match transition with
            DF.GUse new_state -> ignore (printf " propogates %a\n" pretty new_state);
          | DF.GDefault -> ignore (printf " has no special effect\n");
          | _ -> E.s (E.bug "unexpected evaluation of guard\n")
      );
      flush stdout;
      transition
  ;;
    DF.GDefault
  ;;

  let filterStmt _ = 
    true
  ;;

end;;


module Track = DF.ForwardsDataFlow(Apollo_Dataflow);;



(****************************************)
(****************************************)
(* Interface Functions *)
(****************************************)
(****************************************)


let apollo_func (f: fundec) (state: dataflow_state): bool =
    

  IH.clear Apollo_Dataflow.stmtStartData;
  IH.add Apollo_Dataflow.stmtStartData (List.hd f.sbody.bstmts).sid  state;
  Track.compute [List.hd f.sbody.bstmts];

  let return_stmts = MemUtil.get_return_statements f in

    List.iter 
      (fun s -> 
       try 
         let (stores, heaps) = (IH.find Apollo_Dataflow.stmtStartData s.sid) in
           List.iter
             (fun heap ->
                E.error 
                  "Failed to store heap data %a in function %s before return at %a" 
                  d_exp heap f.svar.vname d_loc (get_stmtLoc s.skind)
             )
             heaps
       with Not_found -> 
         E.error "Unable to find state for return statement %d" s.sid;

      )
      return_stmts;

    true
;;


(* Given update a state concisting of stores and heaps to conform to those from
 * a specification with assume_full, assume_empty, and assume_heap.  In
 * particular:
 *
 * - Force any stores listed in assume_full to be full
 * - Force any stores listed in assume_empty to be empty
 * - No update of heaps at this time
 *) 
let set_state_pre (stores, heaps) spec fname = 

  let (assume_full, assume_empty, assume_heap) = lookup_pre spec fname in
      
  if not (List.length assume_heap = 0) then 
    E.s (E.bug "What...  I thought all heap specifications were zero...");

  let match_str_exp s e =
    match e with
        Lval (Var v, _) -> v.vname = s
      | _ -> E.s (E.bug "Unable to match against expression %a" d_exp e)
  in


  let update_state states = 
    List.fold_left
      (fun new_stores (s_state, s_var) ->
         if List.exists (fun s -> match_str_exp s s_var) assume_full then
           (Full, s_var)::new_stores
         else if List.exists (fun s -> match_str_exp s s_var) assume_empty then
           (Empty, s_var)::new_stores
         else
           (s_state, s_var)::new_stores
      )
      []
      states
  in

    (update_state stores, heaps)
;;


