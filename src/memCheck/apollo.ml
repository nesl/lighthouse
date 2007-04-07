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
  type t = mem_states;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "%s" (State.mem_states_to_string state);;


  let computeFirstPredecessor (s: stmt) (state: t): t = state;;


  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) = 
(*
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
 *)
    None
  ;;


  let doInstr (i: instr) (state: t): t DF.action = 
   
    (* 
    ignore (printf "Checking instruction at %a\n" d_loc (get_instrLoc i));
    flush stdout;
    *)

    match i with

        Set (lv, e, _) ->
          begin

            (* Check for invalid memory dereferences *)
            let _ = 
              if invalid_store_dereference e state !current_stmt then (
                  E.error "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_exp e
                    "is dead so it may not appaer on RHS of Set"
                    d_loc (get_stmtLoc !current_stmt.skind)
              );
              if invalid_store_dereference (mkAddrOf lv) state !current_stmt then (
                  E.error "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_lval lv
                    "is dead so it may not be dereferenced"
                    d_loc (get_stmtLoc !current_stmt.skind)
              );
            in

            let lv_state_op = try_lookup_mem_state (Lval lv) state !current_stmt in
            let rh_state_op = try_lookup_mem_state e state !current_stmt in



            match (lv_state_op, rh_state_op) with
                
                (Some (Empty_store, l_key), Some (Full_store er, r_key))
              | (Some (Empty_store, l_key), Some (Nonheap_store er, r_key))
              | (Some (Unknown_store, l_key), Some (Full_store er, r_key))
              | (Some (Unknown_store, l_key), Some (Nonheap_store er, r_key))
              | (Some (Nonheap_store _, l_key), Some (Full_store er, r_key)) 
              | (Some (Nonheap_store _, l_key), Some (Nonheap_store er, r_key)) ->
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, r_key) new_state !current_stmt in
                  let new_state = add_mem_state (Empty_store, r_key) new_state !current_stmt in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                  let new_state = add_mem_state (Full_store er, l_key) new_state !current_stmt in
                    DF.Done new_state
             

              | (Some (Empty_store, l_key), Some (Unknown_store, r_key))
              | (Some (Unknown_store, l_key), Some (Unknown_store, r_key))
              | (Some (Nonheap_store _, l_key), Some (Unknown_store, r_key)) ->
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, r_key) new_state !current_stmt in
                  let new_state = add_mem_state (Empty_store, r_key) new_state !current_stmt in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                  let new_state = 
                    add_mem_state (Full_store IE.nullPtr, l_key) new_state !current_stmt 
                  in
                    DF.Done new_state


              | (Some (Empty_store, l_key), Some (Full_heap er, r_key))
              | (Some (Unknown_store, l_key), Some (Full_heap er, r_key))
              | (Some (Nonheap_store _, l_key), Some (Full_heap er, r_key)) ->
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, r_key) new_state !current_stmt in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                  let new_state = add_mem_state (Full_store er, l_key) new_state !current_stmt in
                    DF.Done new_state


              | (Some (Empty_store, l_key), None)
              | (Some (Unknown_store, l_key), None)
              | (Some (Nonheap_store _, l_key), None) ->
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                  let new_state = add_mem_state (Nonheap_store e, l_key) new_state !current_stmt in
                    DF.Done new_state
                  

              | (Some (Dead_heap el, l_key), Some (Full_store er, r_key))
              | (Some (Dead_heap el, l_key), Some (Nonheap_store er, r_key)) ->
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                    DF.Done new_state


              | (Some (Dead_heap el, l_key), Some (Unknown_store, r_key)) ->
                  let new_state = state in
                  let new_state =
                    add_mem_state (Full_store IE.nullPtr, r_key) new_state !current_stmt
                  in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                    DF.Done new_state


              | (Some (Dead_heap el, l_key), None) ->
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                    DF.Done new_state
              

              | (_, Some (Dead_heap _, _))
              | (_, Some (Empty_store, _)) ->
                  E.error "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_exp e
                    "is empty or dead and may not be dereferenced"
                    d_loc (get_stmtLoc !current_stmt.skind);
                  DF.Done state

              

              | (Some (Full_store er, l_key), _) ->
                  E.error "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_exp (Lval lv)
                    "stores heap data that is overwritten"
                    d_loc (get_stmtLoc !current_stmt.skind);
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                  let new_state = add_mem_state (Error_store, l_key) new_state !current_stmt in
                    DF.Done new_state
             

              | (Some (Full_heap er, l_key), _) ->
                  E.error "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_exp (Lval lv)
                    "stores heap data that is overwritten"
                    d_loc (get_stmtLoc !current_stmt.skind);
                  let new_state = state in
                  let new_state = remove_mem_state (Dummy, l_key) new_state !current_stmt in
                  let new_state = add_mem_state (Error_heap er, l_key) new_state !current_stmt in
                    DF.Done new_state
             

              | (_, Some (Error_store, _))
              | (_, Some (Error_heap _, _)) ->
                  E.s (E.error "%s %a %s at %a"
                         "Apollo.doInstr:"
                         d_exp e
                         "is in an error state and can not be used"
                         d_loc (get_stmtLoc !current_stmt.skind))
                
                  
              | (Some (Error_store, _), _)
              | (Some (Error_heap _, _), _) ->
                  E.s (E.error "%s %a %s at %a"
                         "Apollo.doInstr:"
                         d_exp (Lval lv)
                         "is in an error state and can not be used"
                         d_loc (get_stmtLoc !current_stmt.skind))


              | (None, Some (Full_store _, r_key))
              | (None, Some (Unknown_store, r_key))
              | (None, Some (Nonheap_store _, r_key))
                  when (is_field_of_mem_state (Lval lv) state !current_stmt) -> 
                  
                  E.warn "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_exp e
                    "is stored into nested store and no longer tracked"
                    d_loc (get_stmtLoc !current_stmt.skind);

                  let new_state = state in 
                  let new_state = remove_mem_state (Dummy, r_key) new_state !current_stmt in
                  let new_state = add_mem_state (Empty_store, r_key) new_state !current_stmt in
                  DF.Done new_state
                

              | (None, Some (Full_heap er, r_key))
                  when (is_field_of_mem_state (Lval lv) state !current_stmt) -> 
                  
                  E.warn "%s %a %s at %a"
                    "Apollo.doInstr:"
                    d_exp e
                    "is stored into nested store and no longer tracked"
                    d_loc (get_stmtLoc !current_stmt.skind);
                  
                  let new_state = state in 
                  let new_state = remove_mem_state (Dummy, r_key) new_state !current_stmt in
                  let new_state = add_mem_state (Dead_heap er, r_key) new_state !current_stmt in
                  DF.Done new_state
                

              (* TODO: Double check the rest of these default cases *)
              | (_, _) -> DF.Default
          
          end


      | Call (lvop, Lval((Var v), NoOffset), el, _) ->

          if !dbg_apollo_i then (
            ignore (printf "State coming into called function %s is:\n%a\n" 
                      v.vname pretty state);
            flush stdout;
          );

          (* Verify that we are in a state that is valid for the function call
           *)
          let proper_pre = 
            verify_state_with_pre v.vname (lvop, el) state !current_stmt
          in

          let _ =
            if not proper_pre then (
              E.error "%s %s %a"
                "Apollo.Apollo_Dataflow.doInstr:"
                "Unsafe precondition found in call" 
                d_instr i
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
          E.bug "Have not implemented this form of call at %a"
            d_loc (get_stmtLoc !current_stmt.skind);
          DF.Default


      | _ -> DF.Default
  ;;


  let doStmt (s: stmt) (state: t) =
    current_stmt := s;

    match s.skind with 
        If (e, _, _, _)
      | Switch (e, _, _, _) ->
          if invalid_store_dereference e state !current_stmt then (
            E.error "%s %a %s at %a"
              "Apollo.doStmt:"
              d_exp e
              "is dead so it may not be dereferenced"
              d_loc (get_stmtLoc !current_stmt.skind)
          );
          DF.SDefault
      | _ -> DF.SDefault
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
        (is_heap_state e1 state !current_stmt) || 
        (is_heap_state e2 state !current_stmt)
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
          when (is_heap_state (Lval lv) state !current_stmt) ->
          (* Unary check to see if item is NOT null.  Continue on with the
           * default action to ensure that the target is stored. *)
          DF.GDefault

      | UnOp (LNot, (Lval lv), _) 
          when (is_heap_state (Lval lv) state !current_stmt) ->
          (* Unary check to see if item is Null.  Since we know that the target is
           * Null we can abort the check for this branch and directly insert the Null
           * state. *)
          DF.GUse (kill_heap_state (Lval lv) state !current_stmt)

      | UnOp (LNot, (UnOp (LNot, (Lval lv), _)), _)
          when (is_heap_state (Lval lv) state !current_stmt) ->
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
          if (is_heap_state e1 state !current_stmt) then
            DF.GUse (kill_heap_state e1 state !current_stmt)
          else
            DF.GUse (kill_heap_state e2 state !current_stmt)
         
      | UnOp (LNot, (BinOp (Ne, e1, e2, _)), _)
          when (is_target_and_null e1 e2 !current_stmt) ->
          (* Binary check to see if item is NOT NOT null.  This form results
           * from the elseGuard clause within the dataflow engine. *)
          if (is_heap_state e1 state !current_stmt) then
            DF.GUse (kill_heap_state e1 state !current_stmt)
          else
            DF.GUse (kill_heap_state e2 state !current_stmt)

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
  let global_stores = List.map (fun e -> (Unknown_store, e)) global_exps in
  let initial_heaps = [] in
  let initial_states = initial_heaps @ global_stores in
  
  let formals = List.map (fun v -> (Lval (var v))) f.sformals in

  (* Update the state based on the pre-condition assumptions that we can assume
   * are true upon having entered a function *)
  let state = 
    State.update_state_with_pre f.svar.vname formals initial_states
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
           if not (verify_state_with_post f.svar.vname (return_eop, formals) return s) 
           then
             E.error 
               "Return at %a fails to satisfy post- conditions for function %s"
               d_loc (get_stmtLoc s.skind) f.svar.vname
       with Not_found -> 
         E.error "Unable to find state for return statement %d" s.sid;

      )
      return_stmts;

    true
;;


