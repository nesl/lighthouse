open Cil;;

module E = Errormsg;;
module IE = IsEquivalent;;

type store_spec = {
  full:string list;
  empty:string list;
  heap:string list;
};;


type spec_block = 
    Stores of string list
  | Pre of (string * store_spec)
  | Post of (string * store_spec)
;;


let lookup_post blocks name =
  let block = 
    try
      List.find 
        (fun b -> 
           match b with 
             | Post (f_name, state) when (f_name = name) -> true
             | _ -> false
        ) 
        blocks
    with 
        Not_found -> 
          E.warn "Failed to find post state for function: %s" name;
          Post (name, {full=[]; empty=[]; heap=[]})
  in
    match block with
        Post (_, state) -> (state.full, state.empty, state.heap)
      | _ -> E.s (E.error "Huh?")
;;


let lookup_pre blocks name =
  let block = 
    try
      List.find 
        (fun b -> 
           match b with 
             | Pre (f_name, state) when (f_name = name) -> true
             | _ -> false
        ) 
        blocks
    with 
        Not_found -> 
          E.warn "Failed to find pre state for function: %s" name;
          Pre (name, {full=[]; empty=[]; heap=[]})
  in

    match block with
        Pre (_, state) -> (state.full, state.empty, state.heap)
      | _ -> E.s (E.error "Huh?")
;;

(* Reference to the pre- / post- condition specifications *)
let specification : spec_block list ref = ref [];;

(* State maintained by the dataflow analysis.  This includes a set of stores and
 * a list of expressions refering to heap data that has not been stored. *)
type store_state = Empty | Full | Nonheap | Unknown | Error;;

type store_data = (store_state * exp);;
type heap_data = exp;;

type dataflow_state = (store_data list) * (heap_data list);;

(** Return true if target expression is equivalent to one of the heap enteries
  * in state *)
let is_heap (target: exp) (state: dataflow_state) (current_stmt: stmt): bool =
  let (_, heaps) = state in

  let b =
    List.exists
      (fun heap -> IE.is_equiv_start target heap current_stmt)
      heaps
  in
    b
;;


(** Retrun true if target expression is equivalent to one of the store enteries
  * in state *)
let is_store (target: exp) (state: dataflow_state) (current_stmt: stmt): bool = 
  let (stores, _) = state in

  let b =
    List.exists
      (fun (_, store) -> IE.is_equiv_start target store current_stmt)
      stores
  in
    b
;;


(** Attempts to fill the store represented by target expression within the
  * state.  Returns an updated state. *)
let fill_store (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Full, e2)
         | (Unknown, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Full, e2)
         | (_, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Overwriting store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
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
let use_store (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Full, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Full, e2)
         | (Unknown, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Full, e2)
         | (_, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Using empty store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
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
let empty_store (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Full, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Empty, e2)
         | (Unknown, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Empty, e2)
         | (_, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Releasing empty store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
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
let abuse_store (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in
  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Nonheap, e2)
         | (_, e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             ignore (E.error 
                       "Storing non-heap data into store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
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
let add_heap (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in

    if (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) heaps) then (
      ignore (E.error 
                "Expression already is heap at %a" 
                d_loc (get_stmtLoc current_stmt.skind));
      (stores, heaps)
    ) else
      (stores, target::heaps)
;;


(** Use heap data that is referenced by target expression.  Returns an updated
  * state. *)
let use_heap (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =
  let (stores, heaps) = state in

    if not (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) heaps) then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.use_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      state
;;


(** Remove heap data referenced by target expression from the state.  Returns an
  * updated state. *)
let remove_heap (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in

  let new_heaps = 
    List.filter 
      (fun heap -> not (IE.is_equiv_start target heap current_stmt))
      heaps 
  in

    if (compare new_heaps heaps) = 0 then (
      ignore (E.error 
                "Can not free non-heap expression at %a" 
                d_loc (get_stmtLoc current_stmt.skind));
      (stores, heaps)
    ) else
      (stores, new_heaps)
;;


(** Overwrite heap data with something eles *)
let overwrite_heap (target: exp) (state: dataflow_state) (current_stmt: stmt): dataflow_state =

  let (stores, heaps) = state in

  let new_heaps = 
    List.filter 
      (fun heap -> 
         if (IE.is_equiv_start target heap current_stmt) then (
           ignore (E.error 
                     "Overwriting heap expression at %a" 
                     d_loc (get_stmtLoc current_stmt.skind));
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
let rec get_state_from_exp (e: exp) (state: dataflow_state) (current_stmt: stmt): state_info =
  match e with
    | Lval lv when is_heap (Lval lv) state current_stmt -> Heap (Lval lv)

    | Lval lv when is_store (Lval lv) state current_stmt -> Store (Lval lv)

    | UnOp (_, e, _) -> get_state_from_exp e state current_stmt

    | BinOp (_, e1, e2, _) -> 
        begin
          match (get_state_from_exp e1 state current_stmt, get_state_from_exp e2 state current_stmt) with
              (Other, tmp) -> tmp
            | (tmp, Other) -> tmp
            | _ -> Complex e
        end

    | CastE (_, e) -> get_state_from_exp e state current_stmt

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


(* Update state based on pre-conditions *)
let update_state_with_pre (state: dataflow_state) (current_stmt: stmt): dataflow_state =
  state
;;

(* Verify that state upholds pre-conditions *)
let verify_state_with_pre (state: dataflow_state) (current_stmt: stmt): bool =
  (*
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
  true
;;

(* Update state based on post-conditions *)
let update_state_with_post (state: dataflow_state) (current_stmt: stmt): dataflow_state =
  state
;;

(* Verify that state upholds post-conditions *)
let verify_state_with_post (state: dataflow_state) (current_stmt: stmt): bool =
  true
;;

