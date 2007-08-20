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
  | Unknown
  | Error
;;


(* Pretty print the i2c state *)
let i2c_state_to_string state: string =

  let state_string = match state with
      Reserved -> "reserved"
    | Free -> "free"
    | Unknown -> "unknown"
    | Error -> "error"
  in

    (Pretty.sprint 70 (Pretty.dprintf "%s" state_string))
;;


(** Returns the state (no index) *)
let lookup_i2c_state (state: i2c_state): i2c_state =
  state
;;


(** Update the state (no index) *)
let update_i2c_state (state: i2c_state): i2c_state = 
  state
;;


(* Pre and post state for the system associates an i2c state with a function
 * name. *)
type spec_type = {
  mutable pre: (string * i2c_state) list;
  mutable post: (string * i2c_state) list;
};;


(* Reference to the pre- / post- condition specifications *)
let specification : spec_type ref = 
  ref {pre=[]; post=[]} 
;;

let fsm_specification : spec_type ref = 
  ref {pre=[]; post=[]} 
;;



(* Find the pre- / post- condition for a function *)
let spec_lookup_all (specs: (string * i2c_state) list) (name: string): i2c_state list =
  List.fold_left
    (fun states spec -> match spec with 
         (new_name, new_state) when new_name = name -> new_state::states
       | _ -> states
    )
    []
    specs
;;


let spec_lookup (specs: (string * i2c_state) list) (name: string): i2c_state =
  let states = spec_lookup_all specs name in

    match states with
        [] -> Unknown
      | state::[] -> state
      | hd::tl -> 
          if List.for_all (fun state -> state == hd) tl then
            hd
          else
            E.s (E.error "Found more than one specification for function %s" name)
;;


(* Update state based on pre-conditions *)
let update_state_with_pre (fname: string) (state: i2c_state) (fsm: bool): i2c_state =
  let spec_state = 
    if fsm then
         spec_lookup !fsm_specification.pre fname
    else
         spec_lookup !specification.pre fname 
  in
    spec_state
;;



(* Verify that state upholds on pre-conditions *)
let verify_state_with_pre (fname: string) (state: i2c_state) (s: stmt): bool =
  let spec_state = spec_lookup !specification.pre fname 
  in
  let safe = (state == Unknown || spec_state == state) in
    if not safe then (
      E.warn "Entering function %s:\n    Expected state \"%s\" but found \"%s\" at %a"
        fname 
        (i2c_state_to_string spec_state)
        (i2c_state_to_string state)
        d_loc (get_stmtLoc s.skind) 
    );
    safe
;;


(* Update state based on post-conditions *)
let update_state_with_post (fname: string) (state: i2c_state): i2c_state = 
  let state = spec_lookup !specification.post fname in
    state
;;


(* Verify that state upholds post-conditions *)
let verify_state_with_post (fname: string) (state: i2c_state) (s: stmt) (fsm: bool): bool =
  let spec_states = 
    if fsm then
         spec_lookup_all !fsm_specification.post fname
    else
         spec_lookup_all !specification.post fname 
  in
    
  let safe = (spec_states = [] || 
              List.mem Unknown spec_states || 
              List.mem state spec_states) 
  in

    if not safe then (
      E.warn "Leaving function %s:\n    Found  state \"%s\" but expeted \"%s\" at %a"
        fname 
        (i2c_state_to_string state)
        (List.fold_left 
           (fun str state -> (i2c_state_to_string state) ^ " " ^ str)
           ""
           spec_states
        )
        d_loc (get_stmtLoc s.skind) 
    );
    safe
;;

