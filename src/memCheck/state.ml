open Cil;;

module E = Errormsg;;
module IE = IsEquivalent;;

(* State maintained by the dataflow analysis.  This includes a set of stores and
 * a list of expressions refering to heap data that has not been stored. *)
type runtime_store = 
    Empty of exp
  | Full of exp
  | Nonheap of exp
  | Unknown of exp
  | Error of exp;;
type runtime_heap = exp;;

type runtime_state = {
  stores: runtime_store list;
  heaps: runtime_heap list;
};;

(** Retrun true if target expression is equivalent to one of the store enteries
  * in state *)
let is_store (target: exp) (state: runtime_state) (current_stmt: stmt): bool = 
  List.exists
    (fun s -> match s with
         Empty store
       | Full store
       | Nonheap store
       | Unknown store
       | Error store ->
           IE.is_equiv_start target store current_stmt
    )
    state.stores
;;


(** Return true if target expression is equivalent to one of the heap enteries
  * in state *)
let is_heap (target: exp) (state: runtime_state) (current_stmt: stmt): bool =
  List.exists
    (fun heap -> IE.is_equiv_start target heap current_stmt)
    state.heaps
;;

(** Attempts to fill the store represented by target expression within the
  * state.  Returns an updated state. *)
let fill_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Full e2)
         | (Unknown e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Full e2)
         | (Full e2) 
         | (Nonheap e2) 
         | (Error e2)
             when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Overwriting store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
             (Error target)
         | s -> s
      )
      state.stores
  in

    if (compare new_stores state.stores) = 0 then (
      E.s (E.bug "%s %s %a\n"
             "Apollo.fill_store:"
             "Expression is not a store:"
             d_exp target)
    ) else
      {stores=new_stores; heaps=state.heaps}
;;


(** Attempts to use data refrenced by the store represented by target
  * expression.  Retrurns an updated state.  *)
let use_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_stores =
    List.map
      (fun store -> match store with
           (Full e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Full e2)
         | (Unknown e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Full e2)
         | (Empty e2) 
         | (Nonheap e2) 
         | (Error e2)
             when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Using empty store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
             (Error target)
         | s -> s
      )
      state.stores
  in

    if (compare new_stores state.stores) = 0 then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.use_store:"
             "Expression is not a store:"
             d_exp target)
    else
      {stores=new_stores; heaps=state.heaps}
;;


(** Attempts to release data referenced by the store represented by target
  * expression.  Returns an upadated state. *)
let empty_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_stores =
    List.map
      (fun store -> match store with
           (Full e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Empty e2)
         | (Unknown e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Empty e2)
         | (Empty e2) 
         | (Nonheap e2) 
         | (Error e2)
             when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Releasing empty store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
             (Error target)
         | s -> s
      )
      state.stores
  in

    if (compare new_stores state.stores) = 0 then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.empty_store:"
             "Expression is not a store:"
             d_exp target)
    else
      {stores=new_stores; heaps=state.heaps}
;;


(* TODO: Add cases for Nonheap into the other functions that manipulate the
 * store *)
(** Attempts to write a value not referencing heap data into the store
  * represented by target expression.  Returns an updated state. *)
let abuse_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty e2) when (IE.is_equiv_start target e2 current_stmt) ->
             (Nonheap e2)
         | (Full e2) 
         | (Nonheap e2) 
         | (Unknown e2) 
         | (Error e2)
             when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Storing non-heap data into store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
             (Error target)
         | s -> s
      )
      state.stores
  in

    if (compare new_stores state.stores) = 0 then 
      E.s (E.error "%s %s %a\n"
             "Apollo.abuse_store:"
             "Unable to find overwritten store store:"
             d_exp target)
    else 
      {stores=new_stores; heaps=state.heaps}
;;


(** Add target expression to list of heap data that is not currentnly referenced
  * by a store.  Returns an updated state. *)
let add_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  if (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) state.heaps) then
    E.s (E.error 
              "Expression already is heap at %a" 
              d_loc (get_stmtLoc current_stmt.skind))
  else
    {stores=state.stores; heaps=target::state.heaps}
;;


(** Use heap data that is referenced by target expression.  Returns an updated
  * state. *)
let use_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

    if not (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) state.heaps) then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.use_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      {stores=state.stores; heaps=target::state.heaps}
;;


(** Remove heap data referenced by target expression from the state.  Returns an
  * updated state. *)
let remove_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_heaps = 
    List.filter 
      (fun heap -> not (IE.is_equiv_start target heap current_stmt))
      state.heaps 
  in

    if (compare new_heaps state.heaps) = 0 then 
      E.s (E.error 
             "Can not free non-heap expression at %a" 
             d_loc (get_stmtLoc current_stmt.skind))
    else
      {stores=state.stores; heaps=new_heaps}
;;


(** Overwrite heap data with something eles *)
let overwrite_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

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
      state.heaps 
  in

    if (compare new_heaps state.heaps) = 0 then
      E.s (E.bug "%s %s %a\n"
             "Apollo.remove_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      {stores=state.stores; heaps=new_heaps}
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
let rec get_state_from_exp (e: exp) (state: runtime_state) (current_stmt: stmt): state_info =
  match e with
    | Lval lv when is_heap (Lval lv) state current_stmt -> Heap (Lval lv)

    | Lval lv when is_store (Lval lv) state current_stmt -> Store (Lval lv)

    | UnOp (_, e, _) -> get_state_from_exp e state current_stmt

    | BinOp (_, e1, e2, _) -> 
        begin
          match (get_state_from_exp e1 state current_stmt, 
                 get_state_from_exp e2 state current_stmt) with
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


type spec_store = {
  full:string list;
  empty:string list;
  heap:string list;
};;


type spec_block = 
    Stores of string list
  | Pre of (string * spec_store)
  | Post of (string * spec_store)
;;


(* Reference to the pre- / post- condition specifications *)
let specification : spec_block list ref = ref [];;



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



(* Update state based on pre-conditions *)
(* Given update a state concisting of stores and heaps to conform to those from
 * a specification with assume_full, assume_empty, and assume_heap.  In
 * particular:
 *
 * - Force any stores listed in assume_full to be full
 * - Force any stores listed in assume_empty to be empty
 * - No update of heaps at this time
 *) 
let update_state_with_pre (f: fundec): runtime_state =

  let (assume_full, assume_empty, assume_heap) = 
    lookup_pre !specification f.svar.vname
  in
      
  if not (List.length assume_heap = 0) then 
    E.s (E.bug "What...  I thought all heap specifications were zero...");

  let match_str_exp s e =
    match e with
        Lval (Var v, _) -> v.vname = s
      | _ -> E.s (E.bug "Unable to match against expression %a" d_exp e)
  in

  (* TODO: What about global stores? *) 

  (* TODO: This assumes stores are listed by name and not by number :-( *)
  let full_stores = 
    List.fold_left
      (fun full s -> 
         if s.[0] = '$' then (
           let index = int_of_string  (String.sub s 1 (String.length s - 1)) in
           let formal = List.nth f.sformals index in
             (Full (Lval (var formal)))::full
         ) else (
           try 
             let formal = List.find (fun v -> match_str_exp s (Lval (var v))) f.sformals in 
               (Full (Lval (var formal)))::full
           with Not_found -> 
             E.s (E.bug "Dude.  How can we have a full store that ain't a formal?")
         )
      )
      []
      assume_full
  in
      
  let empty_stores = 
    List.fold_left
      (fun empty s -> 
         if s.[0] = '$' then (
           let index = int_of_string  (String.sub s 1 (String.length s - 1)) in
           let formal = List.nth f.sformals index in
             (Empty (Lval (var formal)))::empty
         ) else (
           try 
             let formal = List.find (fun v -> match_str_exp s (Lval (var v))) f.sformals in 
               (Empty (Lval (var formal)))::empty
           with Not_found -> 
             E.s (E.bug "Dude.  How can we have an empty store that ain't a formal?")
         )
      )
      []
      assume_empty
  in
      
    {stores=(full_stores @ empty_stores); heaps=[]}
;;

(* Verify that state upholds pre-conditions *)
let verify_state_with_pre (state: runtime_state) (current_stmt: stmt): bool =
           
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


(* Update state based on post-conditions *)
let update_state_with_post (f: fundec) (state: runtime_state) (current_stmt: stmt): runtime_state =
  
  let (assume_full, assume_empty, assume_heap) = 
    lookup_post !specification f.svar.vname
  in
      
  if not (List.length assume_heap = 0) then 
    E.s (E.bug "What...  I thought all heap specifications were zero...");

  let match_str_exp s e =
    match e with
        Lval (Var v, _) -> v.vname = s
      | _ -> E.s (E.bug "Unable to match against expression %a" d_exp e)
  in

  let new_state = state in
   
  let new_state = 
    List.fold_left
      (fun new_state s -> 
         if s.[0] = '$' then (
           let index = int_of_string  (String.sub s 1 (String.length s - 1)) in
           let formal = List.nth f.sformals index in
             fill_store (Lval (var formal)) new_state current_stmt 
         ) else (
           try 
             let formal = List.find (fun v -> match_str_exp s (Lval (var v))) f.sformals in 
               fill_store (Lval (var formal)) new_state current_stmt 
           with Not_found -> 
             E.s (E.bug "Dude.  How can we have a full store that ain't a formal?")
         )
      )
      new_state
      assume_full
  in
      
  let new_state = 
    List.fold_left
      (fun new_state s -> 
         if s.[0] = '$' then (
           let index = int_of_string  (String.sub s 1 (String.length s - 1)) in
           let formal = List.nth f.sformals index in
             empty_store (Lval (var formal)) new_state current_stmt 
         ) else (
           try 
             let formal = List.find (fun v -> match_str_exp s (Lval (var v))) f.sformals in 
               empty_store (Lval (var formal)) new_state current_stmt 
           with Not_found -> 
             E.s (E.bug "Dude.  How can we have an empty store that ain't a formal?")
         )
      )
      new_state
      assume_empty
  in

    new_state
;;

(* Verify that state upholds post-conditions *)
let verify_state_with_post (f: fundec) (state: runtime_state) (current_stmt: stmt): bool =
  
  let (assume_full, assume_empty, assume_heap) = 
    lookup_post !specification f.svar.vname
  in
      
  if not (List.length assume_heap = 0) then 
    E.s (E.bug "What...  I thought all heap specifications were zero...");

  let match_str_exp s e =
    match e with
        Lval (Var v, _) -> v.vname = s
      | _ -> E.s (E.bug "Unable to match against expression %a" d_exp e)
  in

  let _ = 
    List.iter
      (fun s -> 
         if s.[0] = '$' then (
           let index = int_of_string  (String.sub s 1 (String.length s - 1)) in
           let formal = List.nth f.sformals index in
             if not (
               List.exists
                 (fun st -> match st with
                      (Full e) when (IE.is_equiv_start (Lval (var formal)) e current_stmt) -> true
                    | _ -> false
                 )
                 state.stores
             ) then 
               E.s (E.bug "Store %s is not full or not found" s)
         ) else (
           try 
             let formal = List.find (fun v -> match_str_exp s (Lval (var v))) f.sformals in 
               if not (
                 List.exists
                   (fun st -> match st with
                        (Full e) when (IE.is_equiv_start (Lval (var formal)) e current_stmt) -> true
                      | _ -> false
                   )
                   state.stores
               ) then
                 E.s (E.bug "Store %s is not full or not found" s)
           with Not_found -> 
             (* TODO: What if s was refering to a global store? *) 
             E.s (E.bug "Dude.  How can we have a full store that ain't a formal?")
         )
      )
      assume_full
  in
  
    
  let _ = 
    List.iter
      (fun s -> 
         if s.[0] = '$' then (
           let index = int_of_string  (String.sub s 1 (String.length s - 1)) in
           let formal = List.nth f.sformals index in
             if not (
               List.exists
                 (fun st -> match st with
                      (Full e) when (IE.is_equiv_start (Lval (var formal)) e current_stmt) -> true
                    | _ -> false
                 )
                 state.stores
             ) then
               E.s (E.bug "Store %s is not empty or not found" s)
         ) else (
           try 
             let formal = List.find (fun v -> match_str_exp s (Lval (var v))) f.sformals in 
               if not (
                 List.exists
                   (fun st -> match st with
                        (Full e) when (IE.is_equiv_start (Lval (var formal)) e current_stmt) -> true
                      | _ -> false
                   )
                   state.stores
               ) then
                 E.s (E.bug "Store %s is not empty or not found" s)
           with Not_found -> 
             (* TODO: What if s was refering to a global store? *) 
             E.s (E.bug "Dude.  How can we have a empty store that ain't a formal?")
         )
      )
      assume_empty
  in
  
  let _ =    
    List.iter
      (fun heap ->
         E.error 
           "Failed to store heap data %a in function %s before return at %a" 
           d_exp heap f.svar.vname d_loc (get_stmtLoc current_stmt.skind)
      )
      state.heaps
  in

  true
;;



