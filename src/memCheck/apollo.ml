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
let dbg_apollo = ref false;;

(* Reference to the current statment *)
let current_stmt = ref (mkEmptyStmt ());;

(* Reference to an error log *)
let error_log = ref "Error Log:\n";;

(* Reference to the file we are working with *)
let cil_file = ref dummyFile;;

(****************************************)
(****************************************)
(* Data  flow implementation *)
(****************************************)
(****************************************)
module Apollo_Dataflow = struct

  (* Vital stats for this dataflow. *)
  let name = "apollo";;
  let debug = dbg_apollo_df;;
  type t = runtime_state;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "%s" (State.state_to_string state);;


  let computeFirstPredecessor (s: stmt) (state: t): t = state;;


  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) = 

    let updated_state = ref false in

    (* Intersect the two heap sets *)
    let out_heaps = 
      List.fold_left
        (fun out old_heap ->
           if (is_heap old_heap new_state s) then (
             old_heap::out
           ) else (
             E.error "Join drops old heap element %a at %a signaling leaked heap data" 
               d_exp old_heap
               d_loc (get_stmtLoc s.skind);
             updated_state := true;
             out
           )
        )
        []
        old.r_heaps
    in
    let out_heaps = 
      List.fold_left
        (fun out new_heap ->
           if (is_heap new_heap {r_stores=[]; r_heaps=out_heaps} s) then (
             new_heap::out
           ) else (
             E.error "Join drops new heap element %a %a signaling leaked heap data"
               d_exp new_heap
               d_loc (get_stmtLoc s.skind);
             updated_state := true;
             out
           )
        )
        []
        new_state.r_heaps
    in

    (* Intesect the two store sets *)
    let out_stores = 
      List.fold_left
        (fun out old_store ->
           let found = match old_store with
               Empty e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Empty e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     new_state.r_stores
                 end
             | Full e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Full e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     new_state.r_stores
                 end
             | Nonheap e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Nonheap e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     new_state.r_stores
                 end
             | Unknown e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Unknown e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     new_state.r_stores
                 end
             | Error e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Error e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     new_state.r_stores
                 end
           in
             if found then (
               old_store::out
             ) else (
               E.error "Mismatched store state at join for store %s %a signaling bad store state" 
                 (store_to_string old_store)
                 d_loc (get_stmtLoc s.skind);
               updated_state := true;
               match old_store with
                   Empty e1 
                 | Full e1
                 | Nonheap e1
                 | Unknown e1
                 | Error e1 -> (Error e1)::out
             )
        )
        []
        old.r_stores
    in
    let out_stores = 
      List.fold_left
        (fun out new_store ->
           let found = match new_store with
               Empty e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Empty e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     out_stores
                 end
             | Full e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Full e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     out_stores
                 end
             | Nonheap e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Nonheap e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     out_stores
                 end
             | Unknown e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Unknown e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     out_stores
                 end
             | Error e1 ->
                 begin
                   List.exists
                     (fun store -> match store with
                          Error e2 when IE.is_equiv_start e1 e2 s -> true
                        | _ -> false
                     )
                     out_stores
                 end
           in
             if found then (
               new_store::out
             ) else (
               E.error "Mismatched store state at join for store %s %a signaling bad store state" 
                 (store_to_string new_store)
                 d_loc (get_stmtLoc s.skind);
               updated_state := true;
               match new_store with
                   Empty e1 
                 | Full e1
                 | Nonheap e1
                 | Unknown e1
                 | Error e1 -> (Error e1)::out
             )
        )
        []
        new_state.r_stores
    in
 
      if !updated_state then (
        E.warn "Apollo.combinePredecessors: Returning updated state";
        Some {r_stores=out_stores; r_heaps=out_heaps}
      ) else (
        None
      )
  ;;


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

              | Heap e when (is_field_of_store (Lval lv) state !current_stmt) ->
                  (* Heap data is accounted for, but can no longer be tracked. *)
                  ignore (E.warn "%s %a %s %a\n" 
                            "No longer tracking heap data"
                            d_exp e
                            "stored at"
                            d_loc (get_stmtLoc !current_stmt.skind)
                  );
                  let new_state = remove_heap e state !current_stmt in
                    DF.Done new_state

              | Heap e when (is_field_of_heap (Lval lv) state !current_stmt) ->
                  (* Heap data is accounted for, but can no longer be tracked. *)
                  ignore (E.warn "%s %a %s %a\n" 
                            "No longer tracking heap data"
                            d_exp e
                            "stored at"
                            d_loc (get_stmtLoc !current_stmt.skind)
                  );
                  let new_state = remove_heap e state !current_stmt in
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

              | Store e when (is_field_of_store (Lval lv) state !current_stmt) ->
                  (* "Transfer" heap data out of store.  It is no longer being
                  * tracked. *)
                  ignore (E.warn "%s %a %s %a\n" 
                            "No longer tracking heap data from store"
                            d_exp e
                            "stored at"
                            d_loc (get_stmtLoc !current_stmt.skind)
                  );
                  let new_state = empty_store e state !current_stmt in
                    DF.Done new_state

              | Store e when (is_field_of_heap (Lval lv) state !current_stmt) ->
                  (* "Transfer" heap data out of store.  It is no longer being
                  * tracked. *)
                  ignore (E.warn "%s %a %s %a\n" 
                            "No longer tracking heap data from store"
                            d_exp e
                            "stored at"
                            d_loc (get_stmtLoc !current_stmt.skind)
                  );
                  let new_state = empty_store e state !current_stmt in
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

          if !dbg_apollo_i then (
            ignore (printf "State coming into called function %s is:\n%a\n" 
                      v.vname pretty state);
            flush stdout;
          );

          (* Verify that we are in a state that is valid for the function call
           *)
          let proper_pre = verify_state_with_pre v.vname state !current_stmt (lvop, el) in

          let _ =
            if not proper_pre then (
              E.s (E.error "%s %s %a"
                     "Apollo.Apollo_Dataflow.doInstr:"
                     "Unsafe precondition found in call %a\n" 
                     d_instr i)
            )
          in

          (* Update state to reflect the effects of the function call *)
          let new_state = update_state_with_post v.vname (lvop, el) state !current_stmt in
          
          if !dbg_apollo_i then (
            ignore (printf "State leaving called function %s is:\n%a\n" 
                      v.vname pretty state);
            flush stdout;
          );
          
        
          if (compare new_state state) = 0 then (
              DF.Default
            ) else (
              DF.Done new_state
            )

      | Call _ ->
          E.s (E.bug "Have not implemented this form of call")

      | _ -> DF.Default
  ;;


  let doStmt (s: stmt) (state: t) =
    current_stmt := s;
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

let apollo_func (f: fundec) (cfile: file) : bool =

  cil_file := cfile;

  (* Collect the global variables together *)

  let global_exps = 
    foldGlobals !cil_file
      (fun s g -> match g with
           GVarDecl (v, l) | GVar (v, _, l) -> (Lval (var v))::s
         | _ -> s
      ) 
      []
  in

  (* Create a dummy "initial_state" that has all stores in an unknown state and
   * an empty heap *)
  let global_stores = List.map (fun e -> Unknown e) global_exps in
  let initial_heaps = [] in
  let initial_state = {State.r_stores=global_stores; State.r_heaps=initial_heaps} in
  
  let formals = List.map (fun v -> (Lval (var v))) f.sformals in

  (* Update the state based on the pre-condition assumptions that we can assume
   * are true upon having entered a function *)
  let state = 
    State.update_state_with_pre f.svar.vname formals initial_state
  in

    if !dbg_apollo then (   
      ignore (printf "State coming into function %s is:\n%a\n" 
                f.svar.vname Apollo_Dataflow.pretty state);
      flush stdout;
    );


  (* Run dataflow for function *)
  IH.clear Apollo_Dataflow.stmtStartData;
  IH.add Apollo_Dataflow.stmtStartData (List.hd f.sbody.bstmts).sid  state;
  Track.compute [List.hd f.sbody.bstmts];

  let return_stmts = MemUtil.get_return_statements f in

    if !dbg_apollo then (   
      ignore (printf "State leaving function %s is:\n%a\n" 
                f.svar.vname Apollo_Dataflow.pretty state);
      flush stdout;
    );


  (* For each return point verify that the post conditions required by the
   * function are satisfied by the state *)
    List.iter 
      (fun s -> 
       try
         let return = (IH.find Apollo_Dataflow.stmtStartData s.sid) in 
         let return_eop = 
           match s.skind with
               Return (eop, _) -> eop
             | _ -> None
         in
           if not (State.verify_state_with_post f.svar.vname return_eop formals return s) then
             E.error 
               "Return at %a fails to satisfy post- conditions for function %s"
               d_loc (get_stmtLoc s.skind) f.svar.vname
       with Not_found -> 
         E.error "Unable to find state for return statement %d" s.sid;

      )
      return_stmts;

    true
;;


