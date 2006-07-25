(*
 * File: mustAlias
 * Date: 7/06
 * Purpose: Genereate and query must alias information about a program
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Generate and query must alias information about expressions within a
  * program. *)

(** {b Debugging} Options to enable debugging of the mustAlias module. *)

(** Enable instruction level debugging of the dataflow *)
val dbg_must_i : bool ref

(** Enable debugging of merges within the dataflow *)
val dbg_must_combine : bool ref

(** Enable debugging of the underlying dataflow framework *)
val dbg_must_df : bool ref


(** {b Alias Type} Base type used to describe alias information. *)

(** Type used to describe alias information *)
type alias = 
  
  Next of Cil.exp 
  (** Expression that must be aliased by the expression in question *)

  | End 
  (** Expression in question must alias an end value such as an integer *)

  | Null
  (** Special case of End in which the expression in question must alias Null *) 
      
  | Dead
  (** Expression in question either has no alias information at this point or
    * may point to more than one target *)

      
(** {b Must Alias Functions} The following functions allow application code to
  * interact with the must alias analysis.  Note that [generate_must_alias]
  * should be called for a given function before any of the other calls are
  * made. *)

(** Create the must alias information for a function *)
val generate_must_alias : Cil.fundec -> unit

(** Query the must alias information to see if expression e1 must alias
  * expression e2 *)
val must_alias : Cil.exp -> Cil.exp -> int -> bool                                      

(** Get the must alias information describing what the expression in question
  * must alias *)
val get_alias : Cil.exp -> int -> alias

(** Print the alias information valid at the end of the given statement ID *)
val print_alias : int -> unit


                                      
