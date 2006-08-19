(*
 * File: memUtil
 * Date: 7/06
 * Purpose: Collection of utility functions 
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Enable verbose debugging output from calls to may_aliasWwrapper *)
val dbg_may_alias: bool ref

(** Collection of utility functions *)

(** Checks to see if the first expression expression [sub] is a subexpression
  * contained within the second expression [e].
  *)
val is_subexpression_of: Cil.exp -> Cil.exp -> bool

(** Wraps Ptranal.may_alias to return false when the Ptranal.may_alias throws a
  * "Not_found" exception.  Also adds optional debugging information enabled by
  * setting the [dbg_may_alias] flag to true. *)
val may_alias_wrapper: Cil.exp -> Cil.exp -> bool

val get_claim: Cil.instr -> Cil.exp list

val get_released: Cil.instr -> Cil.exp list
