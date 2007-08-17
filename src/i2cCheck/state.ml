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
;;


(* Pretty print the i2c state *)
let i2c_state_to_string state: string =

  let state_string = match state with
      Reserved -> "reserved"
    | Free -> "free"
    | Unknown -> "unknown"
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


(* Find the pre- / post- condition for a function *)
let spec_lookup (specs: (string * i2c_state) list) (name: string): i2c_state =
  let states = 
    List.fold_left
      (fun states spec -> match spec with 
           (new_name, new_state) when new_name = name -> new_state::states
         | _ -> states
      )
      []
      specs
  in

    match states with
        state::[] -> state
      | [] -> Unknown
      | _ -> E.s (E.error "Fonud more than one specification for function %s" name)
;;

let spec_lookup_pre spec name = spec_lookup spec.pre name ;;

let spec_lookup_post spec name = spec_lookup spec.post name ;;





(* Update state based on pre-conditions *)
let update_state_with_pre (fname: string) (state: i2c_state): i2c_state =
  let state = spec_lookup_pre !specification fname in
    state
;;



(* Verify that state upholds on pre-conditions *)
let verify_state_with_pre (fname: string) (state: i2c_state) : bool =
  let spec_state = spec_lookup_pre !specification fname in
    spec_state == state 
;;


(* Update state based on post-conditions *)
let update_state_with_post (fname: string) (state: i2c_state): i2c_state = 
  let state = spec_lookup_post !specification fname in
    state
;;


(* Verify that state upholds post-conditions *)
let verify_state_with_post (fname: string) (state: i2c_state) : bool =
  let spec_state = spec_lookup_post !specification fname in
    spec_state == state
;;

