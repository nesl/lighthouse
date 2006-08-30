(*
 * File: mayAliasWrapper
 * Date: 8/06
 * Purpose: Wrapper for the CIL may alias analysis
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Enable verbose debugging output from calls to may_aliasWwrapper *)
val dbg_may_alias: bool ref;;

(** Collection of utility functions *)

(** Wraps Ptranal.may_alias to return false when the Ptranal.may_alias throws a
  * "Not_found" exception.  Also adds optional debugging information enabled by
  * setting the [dbg_may_alias] flag to true. *)
val may_alias_wrapper: Cil.exp -> Cil.exp -> bool;;

