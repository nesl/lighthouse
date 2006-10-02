(*
 * File: isStored
 * Date: 9/06
 * Purpose: Determine if all data allocated in a function is properly stored.
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Purpose: Determine if all data allocated in a function is properly stored.
  *)

(** {b Debugging} Options to enable debugging of the isDead module. *)

(** Enable verbouse debugging *)
val dbg_is_store_i : bool ref;;
val dbg_is_store_s : bool ref;;
val dbg_is_store_g : bool ref;;
val dbg_is_store_c : bool ref;;


(** {b IsStore Functional Interface} *)

val is_stored_func : Cil.exp -> Cil.fundec -> Cil.exp list -> bool
val is_stored_instr : Cil.exp -> Cil.stmt -> Cil.instr -> Cil.fundec -> Cil.exp list -> bool
  
