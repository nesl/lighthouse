(*
 * File: memUtil
 * Date: 7/06
 * Purpose: Collection of utility functions 
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Collection of utility functions *)

val get_return_statements: Cil.fundec -> Cil.stmt list;;

val sort_and_uniq: 'a list -> 'a list;;

