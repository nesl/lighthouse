(*
 * File: memUtil
 * Date: 7/06
 * Purpose: Collection of utility functions 
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

val dbg_mem_util: bool ref
val dbg_may_alias: bool ref
val dbg_takes_data: bool ref

(** Collection of utility functions *)
val mayAliasWrapper: Cil.exp -> Cil.exp -> bool

val getVarinfoFromExp: Cil.exp -> Cil.varinfo option

val getOwn: Cil.instr -> Cil.exp list

val get_released: Cil.instr -> Cil.exp list
