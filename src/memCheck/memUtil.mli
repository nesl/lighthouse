(*
 * File: memUtil
 * Date: 7/06
 * Purpose: Collection of utility functions 
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Collection of utility functions *)

(** Checks to see if the first expression expression [sub] is a subexpression
  * contained within the second expression [e].
  *)
val is_subexpression_of: Cil.exp -> Cil.exp -> bool;;

val get_claim: Cil.instr -> Cil.exp list;;

val get_released: Cil.instr -> Cil.exp list;;

