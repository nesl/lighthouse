(*
 * File: isEquivalent
 * Date: 7/06
 * Purpose: Genereate and query equivalency information about a program
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Generate and query equivalency information about expressions within a
  * program. *)

(** {b Debugging} Options to enable debugging of the isEquivalent module. *)

(** Enable instruction level debugging of the dataflow *)
val dbg_equiv_i : bool ref;;

(** Enable debugging of merges within the dataflow *)
val dbg_equiv_combine : bool ref;;

(** Enable debugging of incoming statement states *)
val dbg_equiv_stmt_summary : bool ref;;

(** Enable debugging of search for aliases to an expression *)
val dbg_equiv_get_aliases : bool ref;;

(** Enable debugging of quries to get_equiv_set (called by is_equiv) *)
val dbg_equiv_get_equiv_set : bool ref;;

(** Enable debugging of the underlying dataflow framework *)
val dbg_equiv_df : bool ref;;

(** Enable verbose output during dataflow to alert user when the analysis does
  * not understand a construct. *)
val verbose : bool ref;;
  

(** {b Types} Base type used to describe equivalency information. *)

(** Definition of the NULL pointer.  Assumes that this will be zero type cast to
  * an integer pointer.
  *)
val nullPtr : Cil.exp;;


(** {b Must Alias Functions} The following functions allow application code to
  * query the equivalency inforamation for a function.  Note that
  * [generate_equivalency] must be called for a given function before any of the
  * other calls are made. *)

(** Create the equivalency information for a function *)
val generate_equiv : Cil.fundec -> Cil.file -> unit;;

(** Query the equivalency information to see if expression e1 is equivalent in
  * value to expression e2 *)
val is_equiv : Cil.exp -> Cil.exp -> int -> bool;;

(** Return the set of expressions that an expression is equivalent to at a given
  * point in the program
  *)
val get_equiv_set : Cil.exp -> int -> Cil.exp list;;

(** Print the equivalency sets at the end of the given statement ID *)
val print_equiv_sets : int -> unit;;


                                      
