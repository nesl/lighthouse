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

(* State maintained by the dataflow analysis.  This includes a set of stores and
 * a list of expressions refering to heap data that has not been stored. *)
type store_state = Empty | Full | Unknown | Error;;

type store_data = (store_state * exp);;
type heap_data = exp;;

type dataflow_state = (store_data list) * (heap_data list);;

(* Reference to the current statment *)
let currentStmt = ref (mkEmptyStmt ());;


(* Helper functions *)

let is_heap (target: exp) (state: dataflow_state): bool =
  let (_, heaps) = state in
  
  let b =
    List.exists
      (fun heap -> IE.is_equiv_start target heap !currentStmt)
      heaps
  in
    b
;;

let is_store (target: exp) (state: dataflow_state): bool = 
  let (stores, _) = state in
  
  let b =
    List.exists
      (fun (_, store) -> IE.is_equiv_start target store !currentStmt)
      stores
  in
    b
;;


let fill_store (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in
  
  let new_stores =
    List.map
      (fun store -> match store with
           (Empty, e2) when (IE.is_equiv_start target e2 !currentStmt) ->
             found := true;
             (Full, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !currentStmt) ->
             found := true;
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else (
      E.s (E.error "%s %s %a\n"
        "Apollo.fill_store:"
        "Expression is not a store:"
        d_exp target)
    )
;;


let use_store (target: exp) (state: dataflow_state): dataflow_state =
  
  let (stores, heaps) = state in
  let found = ref false in
  
  let new_stores =
    List.map
      (fun store -> match store with
           (Full, e2) when (IE.is_equiv_start target e2 !currentStmt) ->
             found := true;
             (Full, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !currentStmt) ->
             found := true;
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else state
;;


let empty_store (target: exp) (state: dataflow_state): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in
  
  let new_stores =
    List.map
      (fun store -> match store with
           (Full, e2) when (IE.is_equiv_start target e2 !currentStmt) ->
             found := true;
             (Empty, e2)
         | (_, e2) when (IE.is_equiv_start target e2 !currentStmt) ->
             found := true;
             (Error, target)
         | s -> s
      )
      stores
  in

    if !found then (new_stores, heaps)
    else (
      E.s (E.error "%s %s %a\n"
        "Apollo.empty_store:"
        "Expression is not a store:"
        d_exp target)
    )
;;


let add_heap (target: exp) (state: dataflow_state): dataflow_state =
  
  let (stores, heaps) = state in
  
    if (List.exists (fun heap -> IE.is_equiv_start target heap !currentStmt) heaps) then 
      E.s (E.error "%s %s %a\n"
        "Apollo.add_heap:"
        "Expression is already noted as heap data:"
        d_exp target)
    else
      (stores, target::heaps)
;;


let use_heap (target: exp) (state: dataflow_state): dataflow_state =
  let (stores, heaps) = state in
  
    if not (List.exists (fun heap -> IE.is_equiv_start target heap !currentStmt) heaps) then 
      E.s (E.bug "%s %s %a\n"
        "Apollo.use_heap:"
        "Expression is not heap data:"
        d_exp target)
    else
      state
;;


let remove_heap (target: exp) (state: dataflow_state): dataflow_state =
  
  let (stores, heaps) = state in
  
  let new_heaps = 
    List.filter 
      (fun heap -> not (IE.is_equiv_start target heap !currentStmt))
      heaps 
  in

    if (compare new_heaps heaps) = 0 then
      E.s (E.error "%s %s %a\n"
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
             match store with
                 (Empty, e) ->
                   s ^ (sprint 70 (dprintf "Store %a in state Empty\n" d_exp e))
               | (Full, e) ->
                   s ^ (sprint 70 (dprintf "Store %a in state Full\n" d_exp e))
               | (Unknown, e) -> 
                   s ^ (sprint 70 (dprintf "Store %a in state Unknown\n" d_exp e))
               | (Error, e) -> 
                   s ^ (sprint 70 (dprintf "Store %a in state Error\n" d_exp e))
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
    match i with
       
        Set (lv, e, _) ->
          begin
            match get_state_from_exp e state with
                
                Heap e when (is_store (Lval lv) state) ->
                  (* "Transfer" heap data into the store. *)
                  let new_state = fill_store (Lval lv) state in
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
                  
              | Other ->
                  (* This set expression has no effect (and is not effected by)
                   * the current state *)
                  DF.Default
          end
      
     
      | Call (lvop, e, el, _) ->
          let alloced = MemUtil.get_claim_formals i in
          let freed = MemUtil.get_released i in
 
          if (List.length alloced = 0) && 
             (List.length freed = 0) && 
             not (MemUtil.returns_alloc i) then
            DF.Default
          
          else begin
            
            (* Insure that no expression is being released and allocated *)
            let _ = 
              if List.exists 
                   (fun free_e -> 
                      List.exists 
                        (fun alloc_e -> IE.is_equiv_start free_e alloc_e !currentStmt) 
                        alloced
                   ) 
                   freed
              then
                E.s (E.bug "%s %s %a\n"
                       "Apollo.Apollo_Dataflow.doInstr:"
                       "Call attempting to free and / or allocate same expression: %a"
                       d_instr i)
            in

            let new_state = state in

            (* Warn if an empty store is being used as a regular parameter *)
            let new_state =
              List.fold_left
                (fun s e -> 
                   if not (List.exists 
                              (fun alloc -> IE.is_equiv_start e alloc !currentStmt) 
                              alloced) &&
                       not (List.exists 
                              (fun free -> IE.is_equiv_start e free !currentStmt) 
                              freed) &&
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
              if MemUtil.returns_alloc i then (
                match lvop with
                    Some lv when (is_store (Lval lv) new_state) ->
                      fill_store (Lval lv) new_state
                  | Some lv ->
                      add_heap (Lval lv) new_state
                  | None ->
                      E.s (E.bug "%s %s %a\n"
                             "Apollo.Apollo_Dataflow.doInstr:"
                             "Return value of allocated data is not being stored: %a"
                             d_instr i)
              ) else new_state
            in

              DF.Done new_state
          end

      | _ -> DF.Default
  ;;


  let doStmt (s: stmt) (state: t) =
    currentStmt := s;
    ignore (Pretty.printf "%a\n" d_loc (get_stmtLoc s.skind));
    ignore (Pretty.printf "%a\n" pretty state);
    flush stdout;
    DF.SDefault
  ;;

  let doGuard _ _ = 
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
    
  true 
;;



