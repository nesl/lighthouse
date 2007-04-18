(*
 * File: memUtil
 * Date: 7/06
 * Purpose: Collection of utility functions 
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Collection of utility functions *)

(** Checks to see if the first expression expression [parent] is a subexpression
  * contained within the second expression [child].
  *)
val is_parent_of: Cil.exp -> Cil.exp -> bool;;

val get_parents_of: Cil.exp -> Cil.exp list;;

val get_return_statements: Cil.fundec -> Cil.stmt list;;

val sort_and_uniq: 'a list -> 'a list;;

