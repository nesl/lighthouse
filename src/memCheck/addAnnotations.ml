(*
 * File: addAnnotations
 * Date: 2/06, 7/06
 * Purpose: Add annotations to specific function prototypes
 * Author: Roy Shea <roy@cs.ucla.edu>
 *)

open Pretty
open Cil

module E = Errormsg

let dbg_verbose = ref false
let config_file = ref "config.txt"


(* Update gFun's type with attributes *)
let addFunAttributes gFun attributes =

  (* Return a new type that is the old type with an additional attribute set *)
  let newType t attr = 
    match t with
        TVoid a -> TVoid (addAttribute attr a)
      | TInt (x, a ) -> TInt (x, addAttribute attr a)
      | TFloat (x, a) -> TFloat (x, addAttribute attr a)
      | TPtr (x, a) -> TPtr (x, addAttribute attr a)
      | TArray (x, y, a) -> TArray (x, y, addAttribute attr a)
      | TFun (x, y, z, a) -> TFun (x, y, z, addAttribute attr a)
      | TNamed (x, a) -> TNamed (x, addAttribute attr a)
      | TComp (x, a) -> TComp (x, addAttribute attr a)
      | TEnum (x, a) -> TEnum (x, addAttribute attr a)
      | TBuiltin_va_list a -> TBuiltin_va_list (addAttribute attr a)
  in

  (* Seperate new attributes into return and formamls *)
  let (returnAttr, formalAttrL) = match attributes with
      [] -> E.s (E.error "addFunAttributes: needs non-empty list in attributes\n")
    | hd::[] -> (hd,[])
    | hd::tl -> (hd,tl)
  in

  (* Seperate function attributes into return and formals *)
  let (returnType, formalTypeLOp, x, y) = 
    match gFun.vtype with
        TFun (t, Some ftl, x, y) -> (t, Some ftl, x, y)
      | TFun (t, None, x, y) -> (t, None, x, y)
      | _ -> E.s (E.error "addFunAnnotations: needs gFun to be of type TFun\n")
  in

  (* Update the return attributes *)
  let newReturnType = 
    match returnAttr with 
        Some a -> newType returnType (Attr (a, []))
      | None -> returnType
  in


  (* Walk through attribute option list and update the attributes of each variable*)
  let newFormalTypeLOp = match formalTypeLOp with
      Some formalTypeL ->
        if not (List.length formalAttrL = List.length formalTypeL) then
          E.s (E.error "addFunAttributes: Must have same sized formal and attribute lists\n");
        Some (List.map2 
                (fun newAttrOp oldT ->
                   match (newAttrOp, oldT) with
                     | (Some a, (x, y, oldA)) ->
                         (x, y, (addAttribute (Attr (a, [])) oldA))
                     | (None, _) ->
                         oldT
                ) 
                formalAttrL 
                formalTypeL
        )
    | None -> None
  in
    
    (TFun (newReturnType, newFormalTypeLOp, x, y))
;;



(** Given a hash table of annotations [annotTable] and a global variable
  * [global], if [global] is a function variable and has an entry in the hash
  * table then add the annotations to its type.
  *)
let tryAddFunAttributes annotTable global =

  (* Extract fundec and varinfo from global *)
  let changedType = 
    match global with
        GVarDecl (v, _) -> 
          begin
            match v.vtype with
                TFun (t, ftlop, x, y) ->
                  if Hashtbl.mem annotTable v.vname then (
                    let nft =addFunAttributes v (Hashtbl.find annotTable v.vname) in
                      v.vtype <- nft;
                      true
                  ) else (
                    false
                  )
              | _ -> false
          end
      | GFun (f, _) -> 
          begin
            match f.svar.vtype with
                TFun (t, ftlop, x, y) ->
                  if Hashtbl.mem annotTable f.svar.vname then (
                    let nft = addFunAttributes f.svar (Hashtbl.find annotTable f.svar.vname) in
                      setFunctionType f nft;
                      true
                  ) else (
                    false
                  )
              | _ -> false
          end
      | _ -> false
  in

    if !dbg_verbose then (
      if changedType then (
        ignore (printf "Prototype update for functin %a\n" d_global global);
      ) else (
        ignore (printf "Skipping global %a\n" d_global global);
      );
    );

    ()
;;


(* Get annotations from a file. *)
let getAnnotations file =

  (* Open a file and read each line into a list *)
  let read_lines file =
    let ch = open_in file in
    let lines = ref [] in
      try
        while true do
          lines := input_line ch :: !lines
        done;
        assert false
      with
          End_of_file ->
            List.rev !lines
  in

  let annotations = (Hashtbl.create 5) in

  (* Regular expressions to help parse each line of the file *)
  let function_name_re = Str.regexp "^ *\\([a-zA-Z0-9_-]*\\)" in
  let arguments_re = Str.regexp "(\\([a-zA-Z0-9_, -]*\\))" in 
  let split_re = Str.regexp ", *" in 
  
  (* Given a line of a file with format:
   *    function_name(formal_var_action1, formal_var_action2, ...)
   * Extract the function name and list of foraml variable actions.  Add the
   * list of actions to a hashtable indexed by the function name.
   *)
  let annotation_from_string s =
    (try
       ignore (Str.search_forward function_name_re s 0);
       let fun_name = (Str.matched_group 1 s) in

         ignore (Str.search_forward arguments_re s 0);
         let string_args = Str.split split_re (Str.matched_group 1 s) in

         let args = 
           List.rev (
             List.fold_left 
               (fun alist a -> match a with
                    "None" -> None::alist
                  | "release" -> (Some "sos_release")::alist
                  | "may_release" -> (Some "sos_may_release")::alist
                  | "claim" -> (Some "sos_claim")::alist
                  | _ -> 
                      Errormsg.s (
                        Errormsg.error "Unexpected label in %s: \"%s\"\n" file a
                      )
               )
               [] 
               string_args
           )
         in
           Hashtbl.add annotations fun_name args
     with
         Not_found -> ();
    );
    ()
  in

  (* Parse the file and generate the hash table *)
  let config = (read_lines file) in
    List.iter (fun s -> annotation_from_string s) config;
    annotations
;;


(* Feature *)
let feature : featureDescr = {
  fd_name = "addAnnotations";
  fd_enabled = ref true;
  fd_description = "Add annotations to function prototypes.";
  fd_extraopt = 
    [
      ("--config", Arg.String (fun s -> config_file := s),
       "Enable more vebose debugging");
      ("--aa_dbg_verbose", Arg.Unit (fun _ -> dbg_verbose := true),
       "Enable more vebose debugging");
    ];
  fd_doit = 
    (function (f:file) -> 
      let annotations = getAnnotations !config_file in
        iterGlobals f (tryAddFunAttributes annotations)
    );
  fd_post_check = true;
}

