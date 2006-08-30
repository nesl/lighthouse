(*
 * File: callerAllocatesLval
 * Date: 8/06
 * Purpose: Check to see that function [f] is allocating data into the formal
 * variable described by [v]
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** {b Debugging} *)

(** Enable debugging of merges within the dataflow *)
val dbg_caller_allocates_lval_combine : bool ref;;

(** Enable instruction level debugging of the dataflow *)
val dbg_caller_allocates_lval_i : bool ref;;

(** Enable statement level debugging of the dataflow *)
val dbg_caller_allocates_lval_s : bool ref;;


(** {b Query Interface} *)

(* Check to see that function [f] is allocating data into the formal
  * variable described by [v]. *)
val lval_is_allocated: Cil.varinfo -> Cil.fundec -> bool;;


