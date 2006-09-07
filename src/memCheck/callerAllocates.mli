(*
 * File: callerAllocatesLval
 * Date: 8/06
 * Purpose: Check to see that function [f] is allocating data into the formal
 * variable described by [v]
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** {b Debugging} *)

(** Enable instruction level debugging of the dataflow *)
val dbg_caller_allocates_i : bool ref;;

(** Enable statement level debugging of the dataflow *)
val dbg_caller_allocates_s : bool ref;;

(** Enable debugging of merges within the dataflow *)
val dbg_caller_allocates_c : bool ref;;


(** {b Query Interface} *)

(* Check to see that function [f] is allocating data into the formal
  * variable described by [v]. *)
val var_is_allocated: Cil.varinfo -> Cil.fundec -> bool;;


(* Check to see that function [f] is allocating data into the expression
 * returned by the function. *)
val return_is_allocated: Cil.fundec -> bool;;


