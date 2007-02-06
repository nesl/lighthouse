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


          let proper_pre = verify_state_with_pre v.vname state !current_stmt (lvop, el) in

          let _ =
            if not proper_pre then (
              E.s (E.error "%s %s %a"
                     "Apollo.Apollo_Dataflow.doInstr:"
                     "Unsafe precondition found in call %a\n" 
                     d_instr i)
            )
          in


          let new_state = update_state_with_post v.vname (lvop, el) state !current_stmt in
          
            if (compare new_state state) = 0 then (
              ignore (printf "No need to update state for function call: %a\n" d_instr i);
              DF.Default
            ) else (
              ignore (printf "Updating state for function call: %a\n" d_instr i);
              DF.Done new_state
            )

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

let apollo_func (f: fundec) (cfile: file) : bool =

  cil_file := cfile;

  let global_exps = 
    foldGlobals !cil_file
      (fun s g -> match g with
           GVarDecl (v, l) | GVar (v, _, l) -> (Lval (var v))::s
         | _ -> s
      ) 
      []
  in

  let global_stores = List.map (fun e -> Unknown e) global_exps in
  let initial_heaps = [] in
  let initial_state = {State.r_stores=global_stores; State.r_heaps=initial_heaps} in

  let state = 
    State.update_state_with_pre f.svar.vname global_exps initial_state
  in
   
    ignore (printf "State coming into function %s is:\n%a\n" 
              f.svar.vname Apollo_Dataflow.pretty state);
    flush stdout;

  IH.clear Apollo_Dataflow.stmtStartData;
  IH.add Apollo_Dataflow.stmtStartData (List.hd f.sbody.bstmts).sid  state;
  Track.compute [List.hd f.sbody.bstmts];

  let return_stmts = MemUtil.get_return_statements f in

    List.iter 
      (fun s -> 
       try
         let return = (IH.find Apollo_Dataflow.stmtStartData s.sid) in 
           if not (State.verify_state_with_post f.svar.vname global_exps return s) then
             E.error 
               "Return at %a fails to satisfy post- conditions for function %s"
               d_loc (get_stmtLoc s.skind) f.svar.vname
       with Not_found -> 
         E.error "Unable to find state for return statement %d" s.sid;

      )
      return_stmts;

    true
;;


