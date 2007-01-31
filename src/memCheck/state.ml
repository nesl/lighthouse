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
  r_stores: runtime_store list;
  r_heaps: runtime_heap list;
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
    state.r_stores
;;


(** Return true if target expression is equivalent to one of the heap enteries
  * in state *)
let is_heap (target: exp) (state: runtime_state) (current_stmt: stmt): bool =
  List.exists
    (fun heap -> IE.is_equiv_start target heap current_stmt)
    state.r_heaps
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
      state.r_stores
  in

    if (compare new_stores state.r_stores) = 0 then (
      E.s (E.bug "%s %s %a\n"
             "Apollo.fill_store:"
             "Expression is not a store:"
             d_exp target)
    ) else
      {r_stores=new_stores; r_heaps=state.r_heaps}
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
      state.r_stores
  in

    if (compare new_stores state.r_stores) = 0 then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.use_store:"
             "Expression is not a store:"
             d_exp target)
    else
      {r_stores=new_stores; r_heaps=state.r_heaps}
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
      state.r_stores
  in

    if (compare new_stores state.r_stores) = 0 then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.empty_store:"
             "Expression is not a store:"
             d_exp target)
    else
      {r_stores=new_stores; r_heaps=state.r_heaps}
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
      state.r_stores
  in

    if (compare new_stores state.r_stores) = 0 then 
      E.s (E.error "%s %s %a\n"
             "Apollo.abuse_store:"
             "Unable to find overwritten store store:"
             d_exp target)
    else 
      {r_stores=new_stores; r_heaps=state.r_heaps}
;;


(** Add target expression to list of heap data that is not currentnly referenced
  * by a store.  Returns an updated state. *)
let add_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  if (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) state.r_heaps) then
    E.s (E.error 
              "Expression already is heap at %a" 
              d_loc (get_stmtLoc current_stmt.skind))
  else
    {r_stores=state.r_stores; r_heaps=target::state.r_heaps}
;;


(** Use heap data that is referenced by target expression.  Returns an updated
  * state. *)
let use_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

    if not (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) state.r_heaps) then 
      E.s (E.bug "%s %s %a\n"
             "Apollo.use_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      {r_stores=state.r_stores; r_heaps=target::state.r_heaps}
;;


(** Remove heap data referenced by target expression from the state.  Returns an
  * updated state. *)
let remove_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_heaps = 
    List.filter 
      (fun heap -> not (IE.is_equiv_start target heap current_stmt))
      state.r_heaps 
  in

    if (compare new_heaps state.r_heaps) = 0 then 
      E.s (E.error 
             "Can not free non-heap expression at %a" 
             d_loc (get_stmtLoc current_stmt.skind))
    else
      {r_stores=state.r_stores; r_heaps=new_heaps}
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
      state.r_heaps 
  in

    if (compare new_heaps state.r_heaps) = 0 then
      E.s (E.bug "%s %s %a\n"
             "Apollo.remove_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      {r_stores=state.r_stores; r_heaps=new_heaps}
;;


(* State info types used to describe an expression as:
 * Heap -> Heap data
 * Stare -> Store data
 * Complex -> Combination of one or more heap and / or store datas
 * Other -> Nothing of interest such as a constant
 *)
type exp_state = 
   Heap of exp 
  | Store of exp
  | Complex of exp
  | Other
;;

(* This function is used to extract state relevant information from an
 * expression.
 *)
let rec get_state_from_exp (e: exp) (state: runtime_state) (current_stmt: stmt): exp_state =
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


type spec_type = {
  mutable stores: string list;
  mutable pre: (string * spec_store) list;
  mutable post: (string * spec_store) list;
};;

(* Reference to the pre- / post- condition specifications *)
let specification : spec_type ref = ref {stores=[]; pre=[]; post=[]} ;;


let spec_lookup (blocks: (string * spec_store) list) (name: string): spec_store =
  let stores = 
    List.fold_left
      (fun stores block -> match block with 
           (new_name, new_stores) when new_name = name -> new_stores::stores
         | _ -> stores
      )
      []
      blocks
  in

    match stores with
        store::[] -> store
      | [] -> {full=[]; empty=[]; heap=[]}
      | _ -> E.s (E.error "Fonud more than one specification for function %s" name)
;;

let spec_lookup_pre spec name = spec_lookup spec.pre name ;;

let spec_lookup_post spec name = spec_lookup spec.post name ;;



(* Update state based on pre-conditions *)
(* Given update a state concisting of stores and heaps to conform to those from
 * a specification with store.full, store.empty, and store.heap.  In
 * particular:
 *
 * - Force any stores listed in store.full to be full
 * - Force any stores listed in store.empty to be empty
 * - No update of heaps at this time
 *) 
let update_state_with_pre (f: fundec): runtime_state =

  let store = spec_lookup_pre !specification f.svar.vname in
      
  if not (List.length store.heap = 0) then 
    E.s (E.bug "What...  I thought all heap specifications were zero...");

  let match_str_exp s e =
    match e with
        Lval (Var v, _) -> v.vname = s
      | _ -> E.s (E.bug "Unable to match against expression %a" d_exp e)
  in

  (* TODO: What about global stores? *) 

  let full_stores : runtime_store list= 
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
      store.full
  in
      
  let empty_stores : runtime_store list= 
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
      store.empty
  in
      
    {r_stores=(full_stores @ empty_stores); r_heaps=[]}
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


(* Update state based on post-conditions *)
let update_state_with_post (f: fundec) (state: runtime_state) (current_stmt: stmt): runtime_state =
  
  let store = spec_lookup_post !specification f.svar.vname in
      
  if not (List.length store.heap = 0) then 
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
      store.full
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
      store.empty
  in

    new_state
;;

(* Verify that state upholds post-conditions *)
let verify_state_with_post (f: fundec) (state: runtime_state) (current_stmt: stmt): bool =
  
  let store = spec_lookup_post !specification f.svar.vname in
      
  if not (List.length store.heap = 0) then 
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
                 state.r_stores
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
                   state.r_stores
               ) then
                 E.s (E.bug "Store %s is not full or not found" s)
           with Not_found -> 
             (* TODO: What if s was refering to a global store? *) 
             E.s (E.bug "Dude.  How can we have a full store that ain't a formal?")
         )
      )
      store.full
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
                 state.r_stores
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
                   state.r_stores
               ) then
                 E.s (E.bug "Store %s is not empty or not found" s)
           with Not_found -> 
             (* TODO: What if s was refering to a global store? *) 
             E.s (E.bug "Dude.  How can we have a empty store that ain't a formal?")
         )
      )
      store.empty
  in
  
  let _ =    
    List.iter
      (fun heap ->
         E.error 
           "Failed to store heap data %a in function %s before return at %a" 
           d_exp heap f.svar.vname d_loc (get_stmtLoc current_stmt.skind)
      )
      state.r_heaps
  in

  true
;;



