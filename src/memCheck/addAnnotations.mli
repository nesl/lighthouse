(*
 * File: addAnnotations
 * Date: 7/06
 * Purpose: Add annotations to specific function prototypes
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

(** Manipulate variable attributes *)

(** Configuration file to use.  This defaults to config.txt *)
val config_file : string ref

(** Feature description for module. *)
val feature : Cil.featureDescr

