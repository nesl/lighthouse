(*
 * File: makeOneCFG
 * Date: 9/06
 * Purpose: Force a program to have (about) one instruction in each CIL
 * statement.
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Traverse a Cil.file and seperate each instruction into its own Cil.stmt.
  * This is usefull for dataflow analysis that want to work upon the granularity
  * of instructions. *)
val make_one_cfg : Cil.file -> unit;;

