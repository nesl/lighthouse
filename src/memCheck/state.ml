open Cil;;

module E = Errormsg;;
module IE = IsEquivalent;;

let dbg_state = ref false;;

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

let state_to_string state = 
  let s = 
    List.fold_left 
      (fun s store -> 
         let (type_name, e) = match store with
             (Empty e) -> ("Empty", e)
           | (Full e) -> ("Full", e)
           | (Nonheap e) -> ("Nonheap", e)
           | (Unknown e) -> ("Unknown", e)
           | (Error e) -> ("Error", e)
         in
           s ^ (Pretty.sprint 70 (Pretty.dprintf "Store %a in state %s\n" d_exp e type_name))
      ) 
      "Stores:\n" 
      state.r_stores 
  in
  let s =
    List.fold_left 
      (fun s heap -> 
         s ^ (Pretty.sprint 70 (Pretty.dprintf "Heap referenced by expression %a\n" d_exp heap))
      ) 
      (s ^ "Heap Data:\n") 
      state.r_heaps 
  in
    s
;;


(* A valid form of storing an expression e is to store it:
 * 
 * - directly into a store s such as:
 *     s = e
 * - into a field of a store s such as either of:
 *     s.a = e
 *     s->b = e
 * 
 * This function is used to see if an expression is either one of a set of
 * targets OR a member of a field of a target.
 *)
let is_equiv_to_field_of (e: exp) (targets: exp list) (s: stmt) : bool =

  (* Return simpler sub-expressions of the expression e.  But only those
   * sub-expressions with the same level of memory derefencing.  Does that make
   * any sense...  *)
  let rec reduce_expression (e:exp) : exp list =

    let reduced = match e with
      | Lval (Var v, NoOffset) -> [e]

      | Lval (Var v, Field _) 
      | Lval (Var v, Index _) -> [Lval (var v)]

      | Lval (Mem e, NoOffset) ->
          let addrs = reduce_expression e in
          let aliases = 
            List.fold_left 
              (fun l e -> (IE.get_equiv_set_start e s) @ l)
              [] addrs
          in
          let cleaned = MemUtil.sort_and_uniq aliases in
              
            List.map (fun e -> Lval (mkMem ~addr:e ~off:NoOffset)) cleaned

      | Lval (Mem e, Field _)
      | Lval (Mem e, Index _) -> 
          (Lval (Mem e, NoOffset))::(reduce_expression (Lval (Mem e, NoOffset)))

      | BinOp (PlusPI, e, _, _)
      | BinOp (IndexPI, e, _, _)
      | BinOp (MinusPI, e, _, _) -> e::(reduce_expression e)

      | BinOp (PlusA, e, c, _)
      | BinOp (MinusA, e, c, _) when (isConstant c) -> e::(reduce_expression e)

      | BinOp (PlusA, c, e, _)
      | BinOp (MinusA, c, e, _) when (isConstant c) -> e::(reduce_expression e)

      | BinOp (PlusA, e1, e2, _) 
      | BinOp (MinusA, e1, e2, _)
      | BinOp (MinusPP, e1, e2, _) -> e1::e1::(reduce_expression e1)@(reduce_expression e2)

      | UnOp (_, e, _) -> e::(reduce_expression e)

      | _ -> [e]
    in

    let reduced = List.map stripCasts reduced in
      
      if !dbg_state then (
        ignore (Pretty.printf "Reduced expression %a to:\n" d_exp e);
        List.iter (fun e -> ignore (Pretty.printf "    -> %a\n" d_exp e)) reduced;
        flush stdout;
      );

      reduced
  in

  let l0 = ref [] in
  let l1 = ref [] in

    l0 := IE.get_equiv_set_start e s;
    l1 := List.fold_left (fun l e -> (reduce_expression e) @ l) !l0 !l0;
    l1 := MemUtil.sort_and_uniq !l1;
    l1 := List.fold_left (fun l e -> (IE.get_equiv_set_start e s) @ l) [] !l1;
    l1 := MemUtil.sort_and_uniq !l1;
      
    while not ((compare !l0 !l1) = 0) do
    
      l0 := !l1;
      l1 := List.fold_left (fun l e -> (reduce_expression e) @ l) !l0 !l0;
      l1 := MemUtil.sort_and_uniq !l1;
      l1 := List.fold_left (fun l e -> (IE.get_equiv_set_start e s) @ l) [] !l1;
      l1 := MemUtil.sort_and_uniq !l1;
    
    done;

    (* 
    if true then (
     *)
    if !dbg_state then (
      ignore (Pretty.printf "IsStored.is_equiv_to_field_of: %a is related to:\n" d_exp e);
      List.iter (fun e -> ignore (Pretty.printf "    %a\n" d_exp e)) !l0;
      flush stdout;
    );

    List.exists 
      (fun e -> 
         List.exists (fun t -> MemUtil.is_parent_of t e) targets
      ) 
      !l0
   
     (* 
      List.exists (fun e -> List.mem e targets) !l0
      *)
  
;;


(** Return true if target expression is equivalent to one of the heap enteries
  * in state *)
let is_heap (target: exp) (state: runtime_state) (current_stmt: stmt): bool =
  List.exists
    (fun heap -> IE.is_equiv_start target heap current_stmt)
    state.r_heaps
;;


let is_field_of_heap (target: exp) (state: runtime_state) (current_stmt: stmt): bool = 
  List.exists
    (fun heap -> is_equiv_to_field_of target [heap] current_stmt)
    state.r_heaps
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


(** Use heap data that is referenced by target expression.  Returns an unmodifed
* state. *)
let use_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

    if not (List.exists (fun heap -> IE.is_equiv_start target heap current_stmt) state.r_heaps) then 
      E.s (E.bug "%s %s %a\n"
             "State.use_heap:"
             "Expression is not heap data:"
             d_exp target)
    else
      {r_stores=state.r_stores; r_heaps=state.r_heaps}
;;


(** Remove heap data referenced by target expression from the state.  Returns an
  * updated state. *)
let remove_heap (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_heaps = 
    List.filter 
      (fun heap -> not (IE.is_equiv_start target heap current_stmt))
      state.r_heaps 
  in

    if (compare new_heaps state.r_heaps) = 0 then (
      E.warn 
        "%s %a. %s"
        "Can not free non-heap expression at"
        d_loc (get_stmtLoc current_stmt.skind)
        "Leaving state unchanged.";
    );

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

    if (compare new_heaps state.r_heaps) = 0 then (
      ignore (E.error 
                "%s %a %s %a\n"
                "Overwriting heap data with non-heap data"
                d_exp target
                "at"
                d_loc (get_stmtLoc current_stmt.skind)
      )
    );

    {r_stores=state.r_stores; r_heaps=new_heaps}
;;


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


let is_field_of_store (target: exp) (state: runtime_state) (current_stmt: stmt): bool = 
  List.exists
    (fun s -> match s with
         Empty store
       | Full store
       | Nonheap store
       | Unknown store
       | Error store ->
           is_equiv_to_field_of target [store] current_stmt
    )
    state.r_stores
;;



let lookup_store_by_name (sname: string) (state: runtime_state): exp = 
  let exps = 
    List.fold_left
      (fun exps s -> match s with
           Empty (Lval (Var v, NoOffset))
         | Full (Lval (Var v, NoOffset))
         | Nonheap (Lval (Var v, NoOffset))
         | Unknown (Lval (Var v, NoOffset))
         | Error (Lval (Var v, NoOffset)) when v.vname = sname -> 
             (Lval (Var v, NoOffset))::exps
         | _ -> exps
      )
      []
      state.r_stores
  in

    match exps with
        e::[] -> e
      | [] -> E.s (E.error "Unable to find store with name %s" sname)
      | _ -> E.s (E.error "Found more than one store with name %s" sname)
;;



(** Attempts to fill the store represented by target expression within the
  * state.  Returns an updated state. *)
let fill_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty e2) 
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

    (* Data must be going into a tmp variable that will later be placed in a
     * store. *)
    if (compare new_stores state.r_stores) = 0 then (
      if !dbg_state then (
        ignore (Pretty.printf "State.fill_store adding heap data at: %a" 
                  d_loc (get_stmtLoc current_stmt.skind));
      );
      add_heap target state current_stmt
    ) else
      {r_stores=new_stores; r_heaps=state.r_heaps}
;;


(** Attempts to use data refrenced by the store represented by target
  * expression.  Retrurns an updated state.  *)
let use_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Full e2) 
         | (Unknown e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Full e2)
         | (Empty e2) 
         | (Nonheap e2) 
         | (Error e2) when (IE.is_equiv_start target e2 current_stmt) ->
             ignore (E.error 
                       "Using empty store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
             found := true;
             (Error target)
         | s -> s
      )
      state.r_stores
  in

    if !found then 
      {r_stores=new_stores; r_heaps=state.r_heaps}
    else 
      E.s (E.bug "%s %s %a %a\n"
             "State.use_store:"
             "Expression is not a store:"
             d_exp target d_stmt current_stmt)
;;


(** Attempts to release data referenced by the store represented by target
  * expression.  Returns an upadated state. *)
let empty_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let new_stores =
    List.map
      (fun store -> match store with
           (Full e2) 
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

    if (compare new_stores state.r_stores) = 0 then (
      if !dbg_state then (
        ignore (Pretty.printf "State.empty_store checking heap data at: %a" 
                  d_loc (get_stmtLoc current_stmt.skind));
      );
      remove_heap target state current_stmt
    ) else
      {r_stores=new_stores; r_heaps=state.r_heaps}
    
;;


(* TODO: Add cases for Nonheap into the other functions that manipulate the
 * store *)
(** Attempts to write a value not referencing heap data into the store
  * represented by target expression.  Returns an updated state. *)
let abuse_store (target: exp) (state: runtime_state) (current_stmt: stmt): runtime_state =

  let found = ref false in

  let new_stores =
    List.map
      (fun store -> match store with
           (Empty e2) 
         | (Unknown e2) 
         | (Nonheap e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             (Nonheap e2)
         | (Full e2)
         | (Error e2) when (IE.is_equiv_start target e2 current_stmt) ->
             found := true;
             ignore (E.error 
                       "Storing potential heap reference into store at %a" 
                       d_loc (get_stmtLoc current_stmt.skind));
             (Error target)
         | s -> s
      )
      state.r_stores
  in

    if not !found then (
      E.error "%s %a %s %a\n"
        "Placing nonheap data"
        d_exp target
        "into store at"
        d_loc (get_stmtLoc current_stmt.skind)
    );
      
    {r_stores=new_stores; r_heaps=state.r_heaps}
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


let get_exp_from_str (fname: string) (s:string) ((return: exp option), (el: exp list)) (state: runtime_state): exp =

    if s = "$return" then (
      match return with
          Some e -> e
        | None -> E.s (E.error "Instruction needs a return value in function: %s" fname)
    ) else if s.[0] = '$' then (
      List.nth el (int_of_string (String.sub s 1 (String.length s - 2)) - 1)
    ) else (
      lookup_store_by_name s state 
    )
;;

(* Update state based on pre-conditions *)
(* Given update a state concisting of stores and heaps to conform to those from
 * a specification with store.full, store.empty, and store.heap.  In
 * particular:
 *
 * - Force any stores listed in store.full to be full
 * - Force any stores listed in store.empty to be empty
 * - No update of heaps at this time
 *) 
let update_state_with_pre (fname: string) (formals: exp list) (state): runtime_state =


  let store = spec_lookup_pre !specification fname in
      
  if not (List.length store.heap = 0) then 
    E.s (E.bug "What...  I thought all heap specifications were zero...");

  let full_stores = 
    List.fold_left
      (fun full s -> 
         let store = (get_exp_from_str fname s (None, formals) state) in
           (Full store)::full
      )
      []
      store.full
  in
  
  let empty_stores = 
    List.fold_left
      (fun empty s -> 
         let store = (get_exp_from_str fname s (None, formals) state) in
           (Empty store)::empty
      )
      []
      store.empty
  in
      
      
    {r_stores=(full_stores @ empty_stores); r_heaps=[]}
;;


let is_full (state: runtime_state) (current_stmt: stmt) (store: exp) = 
  let in_store =
    List.exists
      (fun st -> match st with
           (Full e) when (IE.is_equiv_start store e current_stmt) -> true
         | _ -> false
      )
      state.r_stores
  in

  let in_heap =
    List.exists
      (fun e -> IE.is_equiv_start store e current_stmt)
      state.r_heaps
  in

  let is_null = 
    IE.is_equiv_start store IE.nullPtr current_stmt 
  in
    
    (in_store || in_heap || is_null)

;;

let is_empty (state: runtime_state) (current_stmt: stmt) (store: exp) = 

  let not_full_store = 
    List.for_all
      (fun st -> 
         match st with
             (Empty e) when (IE.is_equiv_start store e current_stmt) -> true
           | (Full e) when (IE.is_equiv_start store e current_stmt) -> false
           | (Nonheap e) when (IE.is_equiv_start store e current_stmt) -> false
           | (Unknown e) when (IE.is_equiv_start store e current_stmt) -> true
           | (Error e) when (IE.is_equiv_start store e current_stmt) -> false
           | _ -> true
      )
      state.r_stores
  in

  let not_heap =
    List.for_all
      (fun e -> not (IE.is_equiv_start store e current_stmt))
      state.r_heaps
  in

    (not_full_store && not_heap)
;;


let eop_of_lvop (lvop: lval option): exp option =
  match lvop with
      Some lv -> Some (Lval lv)
    | None -> None
;;


let verify_state_with_pre 
      (fname: string)
      (state: runtime_state) 
      (current_stmt: stmt)
      (lvop, el): bool =

  let store = spec_lookup_pre !specification fname in
      
  if not (List.length store.heap = 0) then 
    E.s (E.bug "%s %s %s"
           "State.verify_state_with_pre:"
           "Non-empty heap specification in post condition for function"
           fname
    );

  let _ = 
    List.iter
      (fun s -> 
         let store = (get_exp_from_str fname s (eop_of_lvop(lvop), el) state) in
           if not (is_full state current_stmt store) then 
             (E.error "%s %s %s %a%s" 
                "Formal parameter with index"
                (String.sub s 1 ((String.length s) - 2))
                "(in function call at"
                d_loc (get_stmtLoc current_stmt.skind)
                ") is not a full store and not heap data.")
      )
      store.full
  in
  
    
  let _ = 
    List.iter
      (fun s -> 
         let store = (get_exp_from_str fname s (eop_of_lvop(lvop), el) state) in
           if not (is_empty state current_stmt store) then 
             (E.error "Store %a is not empty or not found in call to function %s" d_exp store fname)
      )
      store.empty
  in
 
   (* 
  let _ =    
    List.iter
      (fun heap ->
         E.error 
           "Failed to store heap data %a in function %s before return at %a" 
           d_exp heap fname d_loc (get_stmtLoc current_stmt.skind)
      )
      state.r_heaps
  in
    *)

  true
;;

(* Update state based on post-conditions *)
let update_state_with_post 
      (fname: string)
      ((lvop: lval option), (el: exp list))
      (state: runtime_state) 
      (current_stmt: stmt): runtime_state =
  
  let store = spec_lookup_post !specification fname in

    if not (List.length store.heap = 0) then 
      E.s (E.bug "What...  I thought all heap specifications were zero...");

    let new_state = state in

    let new_state = 
      List.fold_left
        (fun new_state s -> 
           let store = get_exp_from_str fname s (eop_of_lvop(lvop), el) state in
             fill_store store new_state current_stmt
        )
        new_state
        store.full
    in

    let new_state = 
      List.fold_left
        (fun new_state s -> 
           let store = get_exp_from_str fname s (eop_of_lvop(lvop), el) state in
             empty_store store new_state current_stmt
        )
        new_state
        store.empty
    in

      new_state
;;


(* Verify that state upholds post-conditions *)
let verify_state_with_post 
      (fname: string)
      (return: exp option)
      (formals: exp list)
      (state: runtime_state) 
      (current_stmt: stmt): bool =
  
  let store = spec_lookup_post !specification fname in
      
  if not (List.length store.heap = 0) then 
    E.s (E.bug "%s %s %s"
           "State.verify_state_with_post:"
           "Non-empty heap specification in post condition for function"
           fname
    );

  let _ = 
    List.iter
      (fun s -> 
         let store = (get_exp_from_str fname s (return, formals) state) in
           if not (is_full state current_stmt store) then 
             (E.error "%s %s %s %a" 
                "Formal parameter with index"
                (String.sub s 1 ((String.length s) - 2))
                "is not a full store and not heap data at return point in")
                d_loc (get_stmtLoc current_stmt.skind)
      )
      store.full
  in
  
    
  let _ = 
    List.iter
      (fun s -> 
         let store = (get_exp_from_str fname s (return, formals) state) in
           if not (is_empty state current_stmt store) then 
             (E.error "Store %a is not empty or not found in call to function %s" d_exp store fname)
      )
      store.empty
  in

  let _ =    
    List.iter
      (fun heap ->
         let safe_heap = 
           match return with
               Some e ->
                 (* If the heap is being returned... *)
                 let heap_returned = IE.is_equiv_start heap e current_stmt in

                 (* And if the return value should be full... *)
                 let should_be_full = List.mem "$return" store.full in

                   (heap_returned && should_be_full)

             | None -> false
         in
           
           if not safe_heap then (
             E.error 
               "Failed to store heap data \"%a\" after function %s before return at %a" 
               d_exp heap fname d_loc (get_stmtLoc current_stmt.skind)
           );
      )
      state.r_heaps
  in

    true
;;



