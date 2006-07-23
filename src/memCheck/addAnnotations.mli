(*
 * File: addAnnotations
 * Date: 7/06
 * Purpose: Add annotations to specific function prototypes
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Manipulate variable attributes *)

(** Update function's type with attributes described by the list.
  *)
val addFunAttributes: Cil.varinfo -> string option list -> Cil.typ

(** Read annotations from the specified file into a hashtable.  Lines of the
  * file should be formatted as: 
  *     function_name(formal_var_action1, formal_var_action2, ...)
  * and store a list of string options describing the formal variable attribuets
  * in a hash table indexed by function name.
  *)
val getAnnotations: string -> (string, string option list) Hashtbl.t

(** Feature description for module.
  *)
val feature : Cil.featureDescr

