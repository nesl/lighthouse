open Cil
open Pretty

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

(* State maintained by the dataflow analysis.  This includes a set of stores and
 * a list of expressions refering to heap data that has not been stored. *)
type store_state = Empty | Full | Nonheap | Unknown | Error;;

type store_data = (store_state * exp);;
type heap_data = exp;;

type dataflow_state = (store_data list) * (heap_data list);;

(* Reference to the current statment *)
let current_stmt = ref (mkEmptyStmt ());;

(* Reference to an error log *)
let error_log = ref "Error Log:\n";;

(* Helper functions *)

(** Return true if target expression is equivalent to one of the heap enteries
  * in state *)
let is_heap (target: exp) (state: dataflow_state): bool =
  let (_, heaps) = state in

  let b =
    List.exists
      (fun heap -> IE.is_equiv_start target heap !current_stmt)
      heaps
  in
    b
;;


(** Retrun true if target expression is equivalent to one of the store enteries
  * in state *)
let is_store (target: exp) (state: dataflow_state): bool = 
  let (stores, _) = state in

  let b =
    List.exists
      (fun (_, store) -> IE.is_equiv_start target store !current_stmt)
      stores
  in
    b
;;


(** Attempts to fill the store represented by target expression within the
  * state.  Returns an updated state. *)
let fill_store (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Full, e2)
         | (Unknown, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Full, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             ignore (E.error 
                       "Overwriting store at %a" 
                       d_loc (get_stmtLoc !current_stmt.skind));
             found := true;
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else (
      E.s (E.bug "%s %s %a\n"
             "Apollo.fill_store:"
             "Expression is not a store:"
             d_exp target)
    )
;;


(** Attempts to use data refrenced by the store represented by target
  * expression.  Retrurns an updated state.  *)
let use_store (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Full, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Full, e2)
         | (Unknown, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Full, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             ignore (E.error 
                       "Using empty store at %a" 
                       d_loc (get_stmtLoc !current_stmt.skind));
             found := true;
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else state
;;


(** Attempts to release data referenced by the store represented by target
  * expression.  Returns an upadated state. *)
let empty_store (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Full, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Empty, e2)
         | (Unknown, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Empty, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             ignore (E.error 
                       "Releasing empty store at %a" 
                       d_loc (get_stmtLoc !current_stmt.skind));
             found := true;
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else (
      E.s (E.bug "%s %s %a\n"
             "Apollo.empty_store:"
             "Expression is not a store:"
             d_exp target)
    )
;;


(* TODO: Add cases for Nonheap into the other functions that manipulate the
 * store *)
(** Attempts to write a value not referencing heap data into the store
  * represented by target expression.  Returns an updated state. *)
let abuse_store (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             (Nonheap, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !current_stmt) ->
             found := true;
             ignore (E.error 
                       "Storing non-heap data into store at %a" 
                       d_loc (get_stmtLoc !current_stmt.skind));
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else (
      ignore (E.error "%s %s %a\n"
                "Apollo.abuse_store:"
                "Unable to find overwritten store store:"
                d_exp target);
      state
    )
;;


(** Add target expression to list of heap data that is not currentnly referenced
  * by a store.  Returns an updated state. *)
let add_heap (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in

    if (List.exists (fun heap -> IE.is_equiv_start target heap !current_stmt) heaps) then (
      ignore (E.error 
                "Expression already is heap at %a" 
                d_loc (get_stmtLoc !current_stmt.skind));
      (stores, heaps)
    ) else
      (stores, target::heaps)
;;


(** Use heap data that is referenced by target expression.  Returns an updated
  * state. *)
let use_heap (target: exp) (state: dataflow_state): dataflow_state =
  let (stores, heaps) = state in

    if not (List.exists (fun heap -> IE.is_equiv_start target heap !current_stmt) heaps) then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.use_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      state
;;


(** Remove heap data referenced by target expression from the state.  Returns an
  * updated state. *)
let remove_heap (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in

  let new_heaps = 
    List.filter 
      (fun heap -> not (IE.is_equiv_start target heap !current_stmt))
      heaps 
  in

    if (compare new_heaps heaps) = 0 then (
      ignore (E.error 
                "Can not free non-heap expression at %a" 
                d_loc (get_stmtLoc !current_stmt.skind));
      (stores, heaps)
    ) else
      (stores, new_heaps)
;;


(** Overwrite heap data with something eles *)
let overwrite_heap (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in

  let new_heaps = 
    List.filter 
      (fun heap -> 
         if (IE.is_equiv_start target heap !current_stmt) then (
           ignore (E.error 
                     "Overwriting heap expression at %a" 
                     d_loc (get_stmtLoc !current_stmt.skind));
           false
         ) else true
      )
      heaps 
  in

    if (compare new_heaps heaps) = 0 then
      E.s (E.bug "%s %s %a\n"
             "Apollo.remove_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      (stores, new_heaps)
;;



(* State info types used to describe an expression as:
 * Heap -> Heap data
 * Stare -> Store data
 * Complex -> Combination of one or more heap and / or store datas
 * Other -> Nothing of interest such as a constant
 *)
type state_info = 
   Heap of exp 
  | Store of exp
  | Complex of exp
  | Other
;;


(* This function is used to extract state relevant information from an
 * expression.
 *)
let rec get_state_from_exp (e: exp) (state: dataflow_state): state_info =
  match e with
    | Lval lv when is_heap (Lval lv) state -> Heap (Lval lv)

    | Lval lv when is_store (Lval lv) state -> Store (Lval lv)

    | UnOp (_, e, _) -> get_state_from_exp e state

    | BinOp (_, e1, e2, _) -> 
        begin
          match (get_state_from_exp e1 state, get_state_from_exp e2 state) with
              (Other, tmp) -> tmp
            | (tmp, Other) -> tmp
            | _ -> Complex e
        end

    | CastE (_, e) -> get_state_from_exp e state

    | AddrOf lv
    | StartOf lv -> Other

    | Lval _                                    
    | Const _ 
    | SizeOf _ 
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _
      -> Other
;;



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
            match get_state_from_exp e state with

                Heap e when (is_store (Lval lv) state) ->
                  (* "Transfer" heap data into the store. *)
                  let new_state = fill_store (Lval lv) state in
                  let new_state = remove_heap e new_state in
                    DF.Done new_state

              | Heap e when (is_heap (Lval lv) state) ->
                  (* Over writing a heap with a reference to another heap *)
                  let new_state = overwrite_heap (Lval lv) state in
                  let new_state = remove_heap e new_state in
                    DF.Done new_state


              | Heap e ->
                  (* Using heap data.  Tranfer depends on use_heap function. *)
                  let new_state = use_heap e state in
                    DF.Done new_state

              | Store e when (is_store (Lval lv) state) ->
                  (* "Transfer" heap data from one store to another. *)
                  let new_state = fill_store (Lval lv) state in
                  let new_state = empty_store e new_state in
                    DF.Done new_state

              | Store e when (is_heap (Lval lv) state) ->
                  (* Over writing a heap with a reference to another heap *)
                  let new_state = overwrite_heap (Lval lv) state in
                  let new_state = use_store e new_state in
                    DF.Done new_state

              | Store e ->
                  (* Using store data.  Transfer depends on use_store function. *)
                  let new_state = use_store e state in
                    DF.Done new_state

              | Complex e ->
                  (* This is something that we do not currently handel. *)
                  E.s (E.bug "%s %s %a\n" 
                         "Apollo.Apollo_Dataflow.doInstr:" 
                         "Unable to update state given set with rval" 
                         d_exp e)

              | Other when (is_store (Lval lv) state) ->
                  (* Over writing a store with some random value *)
                  let new_state = abuse_store e state in
                    DF.Done new_state


              | Other when (is_heap (Lval lv) state) ->
                  (* Over writing a heap with some random value *)
                  let new_state = overwrite_heap e state in
                    DF.Done new_state


              | Other ->
                  (* This set expression has no effect (and is not effected by)
                   * the current state *)
                  DF.Default
          end


      | Call (lvop, e, el, _) ->
          let alloced = MemUtil.get_claim_formals i in
          let freed = MemUtil.get_released i in

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
                    (is_store e state) 
                 then use_store e s
                 else s
              )
              new_state
              el
          in


          (* Update state with newly allocated data *)
          let new_state =  
            List.fold_left 
              (fun s e ->  
                 if (is_store e s) then fill_store e s
                 else add_heap e s
              ) 
              new_state
              alloced
          in


          (* Updated state with freshly released data *)
          let new_state =
            List.fold_left
              (fun s e ->
                 if (is_store e s) then empty_store e s
                 else remove_heap e s
              )
              new_state
              freed
          in


          (* Update state if function returns allocated data *)
          let new_state = 
            match lvop with
                Some lv when (MemUtil.returns_alloc i) &&
                             (is_store (Lval lv) new_state) ->
                  fill_store (Lval lv) new_state
              | Some lv when (MemUtil.returns_alloc i) ->
                  add_heap (Lval lv) new_state
              | None when (MemUtil.returns_alloc i) ->
                  ignore (E.error "%s (%a): %a"
                            "Return value of allocated data is not being stored at "
                            d_loc (get_stmtLoc !current_stmt.skind)
                            d_instr i);
                  new_state
              | Some lv when (is_store (Lval lv) new_state) ->
                  abuse_store (Lval lv) state
              | Some lv when (is_heap (Lval lv) new_state) ->
                  overwrite_heap (Lval lv) state
              | Some lv -> new_state
              | None -> new_state 
          in

            DF.Done new_state

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
        (is_heap e1 state) || 
        (is_heap e2 state)
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
          when (is_heap (Lval lv) state) ->
          (* Unary check to see if item is NOT null.  Continue on with the
           * default action to ensure that the target is stored. *)
          DF.GDefault

      | UnOp (LNot, (Lval lv), _) 
          when (is_heap (Lval lv) state) ->
          (* Unary check to see if item is Null.  Since we know that the target is
           * Null we can abort the check for this branch and directly insert the Null
           * state. *)
          DF.GUse (remove_heap (Lval lv) state)

      | UnOp (LNot, (UnOp (LNot, (Lval lv), _)), _)
          when (is_heap (Lval lv) state) ->
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
          if (is_heap e1 state) then
            DF.GUse (remove_heap e1 state)
          else
            DF.GUse (remove_heap e2 state)
         
      | UnOp (LNot, (BinOp (Ne, e1, e2, _)), _)
          when (is_target_and_null e1 e2 !current_stmt) ->
          (* Binary check to see if item is NOT NOT null.  This form results
           * from the elseGuard clause within the dataflow engine. *)
          if (is_heap e1 state) then
            DF.GUse (remove_heap e1 state)
          else
            DF.GUse (remove_heap e2 state)

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



