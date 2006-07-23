(*
 * File: memUtil
 * Date: 7/06
 * Purpose: Collection of utility functions 
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Collection of utility functions *)

val getVarinfoFromLval: Cil.lval -> Cil.varinfo option
                              
val getVarinfoFromExp: Cil.exp -> Cil.varinfo option

val getOwn: Cil.instr -> Cil.exp list

