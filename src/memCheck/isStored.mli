(*
 * File: isStored
 * Date: 9/06
 * Purpose: Determine if an expression is stored within a function or, given a
 * loctation within a function, before the end of the function.
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Purpose: Determine if an expression is stored within a function or, given a
  * loctation within a function, before the end of the function. *)

(** {b Debugging} Options to enable debugging of the isDead module. *)

(** Enable verbouse output for debugging of instruction level dataflow *) 
val dbg_is_store_i : bool ref;;

(** Enable verbouse output for debuging of statement level dataflow *) 
val dbg_is_store_s : bool ref;;

(** Enable verbose debugging of guard evalutaion within the dataflow *)
val dbg_is_store_g : bool ref;;

(** Enablei verbose output for debugging of joint points *) 
val dbg_is_store_c : bool ref;;


(** {b IsStore Functional Interface} *)

(** Given a target expression [e] and function declaration [f], see if the
  * expression is stored in one of the stores [new_stores]. *)
val is_stored_func : Cil.exp -> Cil.fundec -> Cil.exp list -> bool;;

(** Given a target expression [e] and location within function [f] specified by
  * instruction [i] and statement [s], see if the expression is sored in one of
  * the stores [new_stores]. *)
val is_stored_inst : 
  Cil.exp -> Cil.stmt -> Cil.instr -> Cil.fundec -> Cil.exp list -> bool;;
  
