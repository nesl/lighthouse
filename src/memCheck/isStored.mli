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
val dbg_is_store : bool ref;;


(** {b IsStore Functional Interface} *)

(** Return list of heap data that is not properly stored in the function *)
val not_stored_exps : Cil.fundec -> Cil.exp list -> Cil.exp list;;

  
