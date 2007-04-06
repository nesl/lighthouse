open Cil;;

module E = Errormsg;;
module IE = IsEquivalent;;

let dbg_state = ref false;;
  
(* Unique counters for tracking heap data flowing into or out of functions *)
let incoming_heap_counter = ref 0;;
let outgoing_heap_counter = ref 0;;


(*
 * A function's state describes each store's and heap allocation point's state.
 * This provides a map of heap memory accessible (and not accessible) at any
 * point within a program.
 *)

type heap_exp = exp;;

type mem_state_type =
    Empty_store 
  | Full_store of heap_exp
  | Nonheap_store of exp
  | Unknown_store
  | Error_store
  | Full_heap of heap_exp
  | Dead_heap of heap_exp
  | Error_heap of heap_exp
  | Dummy
;;

type mem_state = (mem_state_type * exp);;

type mem_states = mem_state list;;

let mem_state_to_string mem_state: string =

  let (s_type, key) = mem_state in

  let (type_str, eop) = match s_type with
      Empty_store -> ("empty store", None)
    | Full_store e -> ("full store", Some e)
    | Nonheap_store e -> ("non-heap", Some e)
    | Unknown_store -> ("unknowen store", None)
    | Error_store -> ("error store", None)
    | Full_heap e -> ("full heap", Some e)
    | Dead_heap e -> ("dead heap", Some e)
    | Error_heap e -> ("error heap", Some e)
    | Dummy -> E.s (E.bug "State.mem_state_to_string: Dummy state is invalid at runtime.")
  in

    match eop with
        Some e -> 
          (Pretty.sprint 
             70 
             (Pretty.dprintf 
                "%s containing %a keyed by %a" 
                type_str
                d_exp e
                d_exp key 
             )
          )
      | None ->
          (Pretty.sprint 
             70 
             (Pretty.dprintf 
                "%s keyed by %a" 
                type_str
                d_exp key 
             )
          )
;;


let mem_states_to_string (mem_states: mem_states): string =
  let s_out = 
    List.fold_left 
      (fun s_out mem_state -> 
         let mem_state_string = mem_state_to_string mem_state in
           s_out ^ mem_state_string ^ "\n"
      ) 
      ""
      mem_states
  in
    s_out
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


(** Return true if query_key expression is equivalent some of the memory state that
  * is being tracked. *)
let is_mem_state (query_key: exp) (states: mem_states) (current_stmt: stmt): bool =
  List.exists
    (fun state -> 
       let (_, key) = state in
       IE.is_equiv_start query_key key current_stmt)
    states
;;


let is_field_of_mem_state (query_key: exp) (states: mem_states) (current_stmt: stmt): bool = 
  List.exists
    (fun state -> 
       let (_, key) = state in
         is_equiv_to_field_of query_key [key] current_stmt)
    states
;;


let is_heap_state (query_key: exp) (states: mem_states) (current_stmt: stmt): bool =
  List.exists
    (fun state -> match state with
         Full_heap e, key
       | Dead_heap e, key
       | Error_heap e, key when (IE.is_equiv_start query_key key current_stmt) -> 
           true
       | _ -> false
    )
    states
;;


(** Removes mem_state indexed by key from the states being tracked. *)
let remove_mem_state (kill_state: mem_state) (states: mem_states) (current_stmt: stmt): mem_states =
  let (_, query_key) = kill_state in
  let found = ref false in
  let out_states = 
    List.fold_left
      (fun out state -> match state with
           (_, key) when (IE.is_equiv_start query_key key current_stmt) -> 
             found := true;
             out
         | _ -> state::out
      )
      []
      states
  in

    if not !found then (
      E.s (E.bug "%s %a %s at %a"
             "State.remove_mem_state:"
             d_exp query_key 
             "can not be removed since it is not being tracked"
             d_loc (get_stmtLoc current_stmt.skind))
    );

    out_states
;;


(** Adds mem_state indexed by key to the states being tracked *)
let add_mem_state (state: mem_state) (states: mem_states) (current_stmt: stmt): mem_states =
  
  let (_, key) = state in
 
    if is_mem_state key states current_stmt then (
      E.s (E.bug "%s %a %s at %a"
             "State.add_mem_state:"
             d_exp key 
             "can not be added since it is already being tracked"
             d_loc (get_stmtLoc current_stmt.skind))
    );

    state::states
;;


(** Returns the state indexed by key *)
let try_lookup_mem_state (query_key: exp) (states: mem_states) (current_stmt: stmt): mem_state option =

  let matches = 
    List.filter
      (fun state ->
         let (_, key) = state in
           IE.is_equiv_start query_key key current_stmt
      )
      states
  in 
    if (List.length matches = 1) then (
      Some (List.hd matches)
    ) else if (List.length matches = 0) then (
      None
    ) else (
      (* Try to refine the search *)
      let matches = 
        List.filter
          (fun s -> match s with
               _, key when (compare query_key key = 0) -> true
             | _ -> false
          )
          matches
      in
        if (List.length matches = 1) then (
          (* 
           E.warn "%s %a %s at %a"
                 "State.try_lookup_mem_state:"
                 d_exp query_key 
                 "required refinement to match"
                 d_loc (get_stmtLoc current_stmt.skind);
           *)
          Some (List.hd matches)
        ) else (
          E.s (E.bug "%s %a %s at %a"
                 "State.try_lookup_mem_state:"
                 d_exp query_key 
                 "has more than one entry"
                 d_loc (get_stmtLoc current_stmt.skind))
        )
    )
;;


(** Returns the state indexed by key *)
let invalid_store_dereference (query_key: exp) (states: mem_states) (current_stmt: stmt): 
                                                                       bool =

  let matches_field =
    List.filter
      (fun state -> 
         let (_, key) = state in 
           is_equiv_to_field_of query_key [key] current_stmt)
      states
  in

    if (List.length matches_field = 1) then (
      match (List.hd matches_field) with
        | Empty_store, key
        | Error_store, key
        | Dead_heap _, key
        | Error_heap _, key
        | Dummy, key when not (compare query_key key = 0) ->
            true
        | Full_store _, key
        | Nonheap_store _, key 
        | Unknown_store, key
        | Full_heap _, key ->
            false
        | _ -> 
            false
    )
    else if (List.length matches_field = 0) then (
      false
    ) else (
      E.s (E.bug "%s %a %s at %a"
             "State.invalid_store_dereference:"
             d_exp query_key 
             "has more than one entry"
             d_loc (get_stmtLoc current_stmt.skind))
    )

;;


(** Returns the state indexed by key *)
let lookup_mem_state (query_key: exp) (states: mem_states) (current_stmt: stmt): mem_state =

  let state_op = try_lookup_mem_state query_key states current_stmt in
    match state_op with 
        Some state -> state
      | None ->
          E.s (E.bug "%s %a %s at %a"
                 "State.lookup_mem_state:"
                 d_exp query_key 
                 "is not currently being tracked"
                 d_loc (get_stmtLoc current_stmt.skind))
;;


(* Lookup mem_state by name.  Note that this only works for tracked state with
 * simple variable types. *)
let lookup_mem_state_by_name (sname: string) (states: mem_states): mem_state = 
  let matches = 
    List.fold_left
      (fun out state -> match state with
           Empty_store, (Lval (Var v, NoOffset)) 
         | Full_store _, (Lval (Var v, NoOffset))
         | Nonheap_store _, (Lval (Var v, NoOffset))
         | Unknown_store, (Lval (Var v, NoOffset))
         | Error_store, (Lval (Var v, NoOffset))
         | Full_heap _, (Lval (Var v, NoOffset))
         | Dead_heap _, (Lval (Var v, NoOffset))
         | Error_heap _, (Lval (Var v, NoOffset)) when v.vname = sname -> state::out
         | _ -> out
      )
      []
      states
  in

    if (List.length matches = 1) then (
      List.hd matches
    ) else (
      E.s (E.bug "%s %s %s"
             "State.lookup_mem_state_by_name:"
             sname 
             "is not currently being tracked")
    )
;;


let kill_heap_state 
      (query_key: exp) 
      (states: mem_states) 
      (current_stmt: stmt): mem_states =

  let state = lookup_mem_state query_key states current_stmt in
  let new_states = states in
    match state with
        Full_heap e, key ->
          let new_states = remove_mem_state state new_states current_stmt in
          let new_states = add_mem_state (Dead_heap e, key) new_states current_stmt in
            new_states
      | _ ->
          E.s (E.bug "%s %a %s"
                 "State.kill_heap_state:"
                 d_exp query_key
                 "can not be killed since it is not full")
;;

(* Add a state by name to the mem_states.  Note that this only works for tracked state with
 * simple variable types. *)
let add_mem_state_by_name (sname: string) (new_state: mem_state) (states: mem_states): mem_states = 

  let found = 
    List.exists
      (fun state -> match state with
           Empty_store, (Lval (Var v, NoOffset)) 
         | Full_store _, (Lval (Var v, NoOffset))
         | Nonheap_store _, (Lval (Var v, NoOffset))
         | Unknown_store, (Lval (Var v, NoOffset))
         | Error_store, (Lval (Var v, NoOffset))
         | Full_heap _, (Lval (Var v, NoOffset))
         | Dead_heap _, (Lval (Var v, NoOffset))
         | Error_heap _, (Lval (Var v, NoOffset)) when v.vname = sname -> true
         | _ -> false
      )
      states
  in

    if found then (
      E.s (E.bug "%s %s %s"
             "State.update_mem_state_by_name:"
             sname 
             "is already being tracked")
    );

    new_state::states
;;



let remove_mem_state_by_name (sname: string) (states: mem_states): mem_states = 

  let found = ref false in

  let new_states =
    List.fold_left
      (fun out s -> match s with
           Empty_store, (Lval (Var v, NoOffset)) 
         | Full_store _, (Lval (Var v, NoOffset))
         | Nonheap_store _, (Lval (Var v, NoOffset))
         | Unknown_store, (Lval (Var v, NoOffset))
         | Error_store, (Lval (Var v, NoOffset))
         | Full_heap _, (Lval (Var v, NoOffset))
         | Dead_heap _, (Lval (Var v, NoOffset))
         | Error_heap _, (Lval (Var v, NoOffset)) when v.vname = sname ->
             found := true;
             out
         | _ -> 
             s::out

      )
      []
      states
  in

    if not !found then (
      E.s (E.bug "%s %s %s"
             "State.remove_mem_state_by_name:"
             sname 
             "can not be found for removal")
    );

    new_states
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

let get_key_from_spec_str
      (fname: string) 
      (s:string) 
      ((return: exp option), (el: exp list)) 
      (states: mem_states): exp =

  if s = "$return" then (
    match return with
        Some e -> e
      | None -> E.s (E.error "Instruction needs a return value in function: %s" fname)
  ) else if s.[0] = '$' then (
    List.nth el (int_of_string (String.sub s 1 (String.length s - 2)) - 1)
  ) else (
    let (_, key) = lookup_mem_state_by_name s states in
      key
  )
;;


let pre_get_key_from_spec_str
      (fname: string) 
      (s:string) 
      ((return: exp option), (el: exp list)) 
      (states: mem_states): (exp * bool) =

  if s = "$return" then (
    match return with
        Some e -> (e, true)
      | None -> E.s (E.error "Instruction needs a return value in function: %s" fname)
  ) else if s.[0] = '$' then (
    (List.nth el (int_of_string (String.sub s 1 (String.length s - 2)) - 1), true)
  ) else (
    let (_, key) = lookup_mem_state_by_name s states in
      (key, false)
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
let update_state_with_pre (fname: string) (formals: exp list) (states): mem_states =

  let store = spec_lookup_pre !specification fname in
      
  if not (List.length store.heap = 0) then (
      E.s (E.bug "%s %s %s"
             "State.update_state_with_pre:"
             fname
             "attempts to specify heap pre-state.")
  );

  let states = 
    List.fold_left
      (fun out s -> 
         let (key, is_formal) = (pre_get_key_from_spec_str fname s (None, formals) out) in
         let out =
           if not is_formal then remove_mem_state_by_name s out
           else out
         in
         let base_name = "incoming_heap_in_" in
         let extended_name = 
           base_name ^ fname ^ "_" ^ (string_of_int !incoming_heap_counter) 
         in
         let heap = makeVarinfo false extended_name voidPtrType in
           incoming_heap_counter := !incoming_heap_counter + 5;
           add_mem_state_by_name fname (Full_store (Lval (var heap)), key) out
      )
      states
      store.full
  in
  
  let states = 
    List.fold_left
      (fun out s -> 
         let (key, is_formal) = (pre_get_key_from_spec_str fname s (None, formals) out) in
         let out =
           if not is_formal then remove_mem_state_by_name s out
           else out
         in
           add_mem_state_by_name fname (Empty_store, key) out
      )
      states
      store.empty
  in
     
    states 
;;


let is_full (query_key: exp) (states: mem_states) (current_stmt: stmt) = 

  match try_lookup_mem_state query_key states current_stmt with
      Some (Full_store e, key) 
    | Some (Full_heap e, key) ->
        true
    | None when IE.is_equiv_start query_key IE.nullPtr current_stmt -> 
        true
    | None when is_field_of_mem_state query_key states current_stmt ->
        ignore (E.warn "%s %a %s at %a"
             "State.is_full:"
             d_exp query_key 
             "assumed to be reference to nested heap data"
             d_loc (get_stmtLoc current_stmt.skind));
        true
    | _ -> false
;;


let is_empty (query_key: exp) (states: mem_states) (current_stmt: stmt) = 

  match try_lookup_mem_state query_key states current_stmt with
      Some (Empty_store, key) 
    | Some (Unknown_store, key) -> true
    | Some (Dead_heap _, key) ->
        ignore (E.warn "%s %a %s at %a"
             "State.is_empty:"
             d_exp query_key 
             "is dead heap.  Technically it is empty.  But do you really want to use it?"
             d_loc (get_stmtLoc current_stmt.skind));
        true
    | None -> true
    | _ -> false
;;


let eop_of_lvop (lvop: lval option): exp option =
  match lvop with
      Some lv -> Some (Lval lv)
    | None -> None
;;


let verify_state_with_pre 
      (fname: string)
      (lvop, el)
      (states: mem_states) 
      (current_stmt: stmt): bool =

  let meets_pre = ref true in

  let store = spec_lookup_pre !specification fname in
      
  if not (List.length store.heap = 0) then (
      E.s (E.bug "%s %s %s"
             "State.verify_state_with_pre:"
             fname
             "attempts to specify heap pre-state.")
  );


  let _ = 
            
    (* Check for invalid memory dereferences *)
    ignore (
      match lvop with
          Some lv -> 
            if invalid_store_dereference (mkAddrOf lv) states current_stmt then (
              E.error "%s %a %s at %a"
                "Apollo.doInstr:"
                d_lval lv
                "is dead so it may not be dereferenced"
                d_loc (get_stmtLoc current_stmt.skind)
            )
        | None -> ()
    );

    let count = ref 0 in
      List.iter
        (fun e ->
           count := !count + 1;
           (* An formal variable equivalent to a dead / error store may not be
            * passed to a function unless the function assumes that the
            * parameter is empty.  So we assume the function dereferences the
            * formal at some point. *)
           if not (List.exists 
                     (fun s -> 
                        try 
                          let s_index = (String.sub s 1 ((String.length s) - 2)) in
                          let n_index = int_of_string s_index in
                            n_index = !count
                        with 
                            Failure "int_of_string" -> false
                     ) 
                     store.empty
           ) then (
             if invalid_store_dereference e states current_stmt then (
               E.error "%s %a %s at %a"
                 "State.verify_state_with_pre:"
                 d_exp e
                 "is dead so it may not be dereferenced"
                 d_loc (get_stmtLoc current_stmt.skind)
             );
             match try_lookup_mem_state e states current_stmt with
                 None -> ()
               | Some (Empty_store, key)
               | Some (Error_store, key)
               | Some (Dead_heap _, key)
               | Some (Error_heap _, key)
               | Some (Dummy, key) ->
                   E.error "%s %a %s at %a"
                     "State.verify_state_with_pre:"
                     d_exp e
                     "is dead so it may not be dereferenced"
                     d_loc (get_stmtLoc current_stmt.skind)
               | _ -> ()
             
           )
        )
        el
  in


  let _ = 
    List.iter
      (fun s -> 
         let store = 
           get_key_from_spec_str fname s (eop_of_lvop(lvop), el) states
         in
           if not (is_full store states current_stmt) then (
             meets_pre := false;
             E.error "%s %s %s %s at %a" 
               "State.verify_state_with_pre:"
               "Formal parameter"
               (String.sub s 0 ((String.length s) - 1))
               "is not a full store and not heap data"
               d_loc (get_stmtLoc current_stmt.skind)
           );
      )
      store.full
  in
  
    
  let _ = 
    List.iter
      (fun s -> 
         let store = 
           get_key_from_spec_str fname s (eop_of_lvop(lvop), el) states
         in
           if not (is_empty store states current_stmt) then (
             meets_pre := false;
             E.error "%s %s %s %s at %a" 
               "State.verify_state_with_pre:"
               "Formal parameter"
               (String.sub s 0 ((String.length s) - 1))
               "is not empty"
               d_loc (get_stmtLoc current_stmt.skind)
           );
      )
      store.empty
  in
 
    !meets_pre
;;


(* Update state based on post-conditions *)
let update_state_with_post 
      (fname: string)
      ((lvop: lval option), (el: exp list))
      (states: mem_states) 
      (current_stmt: stmt): mem_states =
    

  let store = spec_lookup_post !specification fname in

  if not (List.length store.heap = 0) then (
      E.s (E.bug "%s %s %s"
             "State.update_state_with_pre:"
             fname
             "attempts to specify heap pre-state.")
  );

  let states = 
    List.fold_left
      (fun states s -> 
         let key = (get_key_from_spec_str fname s (eop_of_lvop(lvop), el) states) in
         let state = try_lookup_mem_state key states current_stmt in
        
         let base_name = "outgoing_heap_in_" in
         let extended_name = 
           base_name ^ fname ^ "_" ^ (string_of_int !outgoing_heap_counter) 
         in
         let heap_var = makeVarinfo false extended_name voidPtrType in
         let _ = outgoing_heap_counter := !outgoing_heap_counter + 5 in
         let heap_exp = Lval (var heap_var) in
         
           match state with
               Some (Empty_store, key)
             | Some (Unknown_store, key)
             | Some (Nonheap_store _, key) ->
                 let states = remove_mem_state (Dummy, key) states current_stmt in
                 let states = 
                   add_mem_state (Full_store heap_exp, key) states current_stmt
                 in
                   states
             
             | Some (Dead_heap e, key) ->
                 let states = remove_mem_state (Dummy, key) states current_stmt in
                 let states = 
                   add_mem_state (Full_heap heap_exp, key) states current_stmt 
                 in
                   states
             
             | Some _ ->
                 E.error "%s %s %s at %a" 
                   "State.verify_state_with_pre:"
                   fname
                   "is not free to store heap data"
                   d_loc (get_stmtLoc current_stmt.skind);
                 states
             
             | None ->
                 let states = add_mem_state (Full_heap heap_exp, key) states current_stmt in
                   states
      )
      states
      store.full
  in

  let states = 
    List.fold_left
      (fun states s -> 
         let key = (get_key_from_spec_str fname s (eop_of_lvop(lvop), el) states) in
         let state = try_lookup_mem_state key states current_stmt in
           match state with
               Some (Full_store _, key) 
             | Some (Unknown_store, key) ->
                 let states = remove_mem_state (Dummy, key) states current_stmt in
                 let states = add_mem_state (Empty_store, key) states current_stmt in
                   states
             
             | Some (Full_heap e, key) ->
                 let states = remove_mem_state (Dummy, key) states current_stmt in
                 let states = add_mem_state (Dead_heap e, key) states current_stmt in
                   states
             
             | None when is_full key states current_stmt ->
                 ignore (E.warn "%s %a %s at %a"
                           "State.update_state_with_post:"
                           d_exp key 
                           "assumed to be reference to nested heap data that now becomes empty"
                           d_loc (get_stmtLoc current_stmt.skind));
                 states

             | _ ->
                 E.error "%s %a %s at %a" 
                   "State.verify_state_with_pre:"
                   d_exp key
                   "is empty and may not be made empty again"
                   d_loc (get_stmtLoc current_stmt.skind);
                 states
      )
      states
      store.empty
  in

    states
;;


(* Verify that state upholds post-conditions *)
let verify_state_with_post 
      (fname: string)
      ((return: exp option), (formals: exp list))
      (states: mem_states) 
      (current_stmt: stmt): bool =
  
  let meets_post = ref true in
  
  let store = spec_lookup_post !specification fname in
      
  if not (List.length store.heap = 0) then (
      E.s (E.bug "%s %s %s"
             "State.verify_state_with_post:"
             fname
             "attempts to specify heap post-state.")
  );

  
  let _ = 
    List.iter
      (fun s -> 
         let store = get_key_from_spec_str fname s (return, formals) states in
           if not (is_full store states current_stmt) then (
             meets_post := false;
             E.error "%s %s %s %s at %a" 
               "State.verify_state_with_post:"
               "Formal parameter"
               (String.sub s 0 ((String.length s) - 1))
               "is not a full store and not heap data at function return"
               d_loc (get_stmtLoc current_stmt.skind)
           );
      )
      store.full
  in
  
    
  let _ = 
    List.iter
      (fun s -> 
         let store = get_key_from_spec_str fname s (return, formals) states in
           if not (is_empty store states current_stmt) then (
             meets_post := false;
             E.error "%s %s %s %s at %a" 
               "State.verify_state_with_post:"
               "Formal parameter"
               (String.sub s 0 ((String.length s) - 1))
               "is not known to be empty at function return"
               d_loc (get_stmtLoc current_stmt.skind)
           );
      )
      store.empty
  in
 

  let _ =    
    List.iter
      (fun state -> match state with
           Full_heap _, key 
         | Error_heap _, key ->
             begin
               match return with
                   Some ret when IE.is_equiv_start key ret current_stmt &&
                                List.mem "$return" store.full -> ()
                 | _ ->
                     meets_post := false;
                     E.error "%s %a %s at %a" 
                       "State.verify_state_with_post:"
                       d_exp key
                       "referes to non-stored heap data at function return"
                       d_loc (get_stmtLoc current_stmt.skind)
             end
         | _ -> ()
      )
      states
  in
    !meets_post
;;



