(*
 * File: isDead
 * Date: 8/06
 * Purpose: Determine if an expression is treated as dead from a given point in
 * the program forward.
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Purpose: Determine if an expression is treated as dead from a given point in
  * the program forward.
  *)

(** {b Debugging} Options to enable debugging of the isDead module. *)

(** Enable verbouse output for debuging of statement level dataflow *) 
val dbg_is_dead_s : bool ref

(** Enable verbouse output for debugging of instruction level dataflow *) 
val dbg_is_dead_i : bool ref

(** Enablei verbose output for debugging of joint points *) 
val dbg_is_dead_c : bool ref


(** {b Analysis Configuration} Options to configure the analysis *)

(** Loops can cause many false positives concerning the treatment of dead data.
  * The current default behavior of the checker is to withold these identified
  * potential problems, to minimize false positives.  Setting [enable_loop] to
  * true exposes this part of the analysis to the caller.
  *)
val enable_loop : bool ref


(** {b IsDead Functional Interface} *)

(** Given an expression and a statment and line number to describe a point an a
  * program, return false if the expression could be dereferenced before it goes
  * out of scope or before it is made valid again (ie. via a call to a malloc
  * function) *)
val is_dead : Cil.exp -> Cil.stmt -> bool
  
