open Cil;;

module E = Errormsg;;

let dbg_state = ref false;;

(* TODO: set this to false before release *)
let quite = ref true;;
  

(*
 * A function's state describes the pre- and post- state of the I2C associated
 * with the function.
 *)
type i2c_state =
    Reserved
  | Free
;;


(* Pretty print the i2c state *)
let i2c_state_to_string state: string =

  let state_string = match state with
      Reserved -> "reserved"
    | Free -> "free"
  in

    (Pretty.sprint 70 (Pretty.dprintf "%s" type_str))
;;


(** Returns the state (no index) *)
let lookup_i2c_state (state: i2c_state): i2c_state =
  state
;;


(** Update the state (no index) *)
let update_i2c_state (state: i2c): i2c_state = 
  state
;;


(* Pre and post state for the system *)
type spec_type = {
  mutable pre: (string * i2c_state) list;
  mutable post: (string * i2c_state) list;
};;


(* Reference to the pre- / post- condition specifications *)
let specification : spec_type ref = 
  ref {stores=[]; pre=[]; post=[]} 
;;


let fsm_specification : spec_type ref = 
  ref {stores=[]; pre=[]; post=[]} 
;;


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
         let (key, is_formal) = 
           (pre_get_key_from_spec_str fname s (None, formals) out) 
         in
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
           add_mem_state_by_name s (Full_store (Lval (var heap)), key) out
      )
      states
      store.full
  in
  
  let states = 
    List.fold_left
      (fun out s -> 
         let (key, is_formal) = 
           (pre_get_key_from_spec_str fname s (None, formals) out) 
         in
         let out =
           if not is_formal then remove_mem_state_by_name s out
           else out
         in
           add_mem_state_by_name s (Empty_store, key) out
      )
      states
      store.empty
  in
     
    states 
;;



let update_state_with_pre_fsm (fname: string) (formals: exp list) (states): mem_states =

  let store = spec_lookup_pre !fsm_specification fname in
      
  if not (List.length store.heap = 0) then (
      E.s (E.bug "%s %s %s"
             "State.update_state_with_pre:"
             fname
             "attempts to specify heap pre-state.")
  );

  let states = 
    List.fold_left
      (fun out s -> 
         let (key, is_formal) = 
           (pre_get_key_from_spec_str fname s (None, formals) out) 
         in
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
           add_mem_state_by_name s (Full_store (Lval (var heap)), key) out
      )
      states
      store.full
  in
  
  let states = 
    List.fold_left
      (fun out s -> 
         let (key, is_formal) = 
           (pre_get_key_from_spec_str fname s (None, formals) out) 
         in
         let out =
           if not is_formal then remove_mem_state_by_name s out
           else out
         in
           add_mem_state_by_name s (Empty_store, key) out
      )
      states
      store.empty
  in
     
    states 
;;



let must_be_field_of_mem_state 
      (query_key: exp) 
      (states: mem_states) 
      (current_stmt: stmt): bool = 

  List.exists
    (fun state -> 
       let (_, key) = state in
         is_equiv_to_field_of query_key [key] current_stmt)
    states
;;


let must_be_full (query_key: exp) (states: mem_states) (current_stmt: stmt) = 


    match try_lookup_must_mem_state query_key states current_stmt with
        Some (Full_store e, key) 
      | Some (Full_heap e, key) ->
          true
      | None when IE.is_equiv_start query_key IE.nullPtr current_stmt -> 
          true
      | None when must_be_field_of_mem_state query_key states current_stmt ->
          ignore (E.warn "%s %a %s at %a"
                    "State.must_be_full:"
                    d_exp query_key 
                    "assumed to be reference to nested heap data"
                    d_loc (get_stmtLoc current_stmt.skind));
          true
      | _ -> false
;;


let is_empty (query_key: exp) (states: mem_states) (current_stmt: stmt) = 

  match try_lookup_must_mem_state query_key states current_stmt with
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
             match try_lookup_must_mem_state e states current_stmt with
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
           if not (must_be_full store states current_stmt) then (
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
         let state = try_lookup_must_mem_state key states current_stmt in
        
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
                 let states = 
                   add_mem_state (Full_heap heap_exp, key) states current_stmt 
                 in
                   states
      )
      states
      store.full
  in

  let states = 
    List.fold_left
      (fun states s -> 
         let key = (get_key_from_spec_str fname s (eop_of_lvop(lvop), el) states) in
         let state = try_lookup_must_mem_state key states current_stmt in
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
             
             | None when must_be_full key states current_stmt ->
                     if not !quite then 
                 ignore (E.warn "%s %a %s at %a"
                           "State.update_state_with_post:"
                           d_exp key 
                           "assumed to be reference to nested heap data that now becomes empty"
                           d_loc (get_stmtLoc current_stmt.skind));
                 states

             | _ ->
                     if not !quite then 
                 E.error "%s %a %s at %a" 
                   "State.update_state_with_post:"
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
           if not (must_be_full store states current_stmt) then (
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


let verify_state_with_post_fsm
      (fname: string)
      ((return: exp option), (formals: exp list))
      (states: mem_states) 
      (current_stmt: stmt): bool =
  
  let meets_post = ref true in
  
  let store = spec_lookup_post !fsm_specification fname in
      
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
           if not (must_be_full store states current_stmt) then (
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



