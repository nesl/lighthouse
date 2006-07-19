(* 
 * File: addAnnotations
 * Author: Roy Shea <roy@cs.ucla.edu>
 * Date: 2/06
 * Purpose: Add annotations to specific function prototypes
 *)

open Pretty
open Cil

module E = Errormsg

let dbg_verbose = ref false

let addAttributes annotations global =

  (* Extract fundec and varinfo from global *)
  let (fdop, vop) = 
    match global with
        GVarDecl (v, _) -> (None, Some v)
      | GFun (f, _) -> (Some f, Some f.svar)
      | _ -> (None, None)
  in

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

  (* Update the varinfo with a new annotations if needed *)
  let newFunTypeOp = 
    match vop with
        Some v ->
          begin
            match v.vtype with
                TFun (t, ftlop, x, y) ->

                  if Hashtbl.mem annotations v.vname then (

                    let (returnAttr, formalAttrL) = match Hashtbl.find annotations v.vname with
                        [] -> (None,[])
                      | hd::[] -> (hd,[])
                      | hd::tl -> (hd,tl)
                    in

                    let (returnType, formalTypeLOp, x, y) = 
                      match v.vtype with
                          TFun (t, Some ftl, x, y) -> (t, Some ftl, x, y)
                        | TFun (t, None, x, y) -> (t, None, x, y)
                        | _ -> E.s (E.bug "Expecting function type in addAnnotations\n")
                    in

                    let newReturnType = 
                      match returnAttr with 
                          Some a -> newType returnType (Attr (a, []))
                        | None -> returnType
                    in


                    (* Assuming that the length of tl is the same as tlop *)
                    let formalTypeLOp = 
                      match formalTypeLOp with
                          Some formalTypeL ->
                            Some ( 
                              List.map2 
                                (fun newAttrOp oldT ->
                                   match (newAttrOp, oldT) with
                                     | (Some a, (x, y, oldA)) ->
                                         (x, y, (addAttribute (Attr (a, [])) oldA))
                                     | (None, _) ->
                                         oldT
                                ) 
                                formalAttrL formalTypeL
                            )
                        | None -> None
                    in
                      Some (TFun (newReturnType, formalTypeLOp, x, y))
                  ) else (
                    None
                  );
              | _ -> None
          end
      | None -> None
  in


    match (fdop, vop, newFunTypeOp) with
        (_, None, _) -> 
          if !dbg_verbose then
            ignore (printf "Skipping nonglobal function\n");
          ()
      | (_, Some v, None) -> 
          if !dbg_verbose then
            ignore (printf "No prototype update needed for function %s: %a\n" v.vname d_type v.vtype);
          ()
      | (Some f, Some v, Some nft) -> 
          setFunctionType f nft;
          if !dbg_verbose then
            ignore (printf "Prototype update for functin dec %s: %a\n" v.vname d_type v.vtype);
          ()
      | (None, Some v, Some nft) -> 
          v.vtype <- nft;
          if !dbg_verbose then
            ignore (printf "Prototype update for extern func %s: %a\n" v.vname d_type v.vtype);
          ()


(** Get annotations from a file
  * @todo Update this to read from a file rather than hardcode the values
  * *)
let getAnnotations () =
    
  let annotations = (Hashtbl.create 5) in

    Hashtbl.add annotations "ker_malloc" 
      [Some "sos_claim"; None; None];

    Hashtbl.add annotations "sos_blk_mem_alloc" 
      [Some "sos_claim"; None; None; None];

    Hashtbl.add annotations "ker_free" 
      [None; Some "sos_release"];

    Hashtbl.add annotations "sos_blk_mem_free" 
      [None; Some "sos_release"; None];

    Hashtbl.add annotations "post_long" 
      [None; None; None; None; None; Some "sos_may_release"; None];

    Hashtbl.add annotations  "post_link"
      [None; None; None; None; None; Some "sos_may_release"; None; None];

    Hashtbl.add annotations  "post_auto"
      [None; None; None; None; None; Some "sos_may_release"; None; None];

    Hashtbl.add annotations  "post_net"
      [None; None; None; None; None; Some "sos_may_release"; None; None];

    Hashtbl.add annotations  "post_uart"
      [None; None; None; None; None; Some "sos_may_release"; None; None];

    Hashtbl.add annotations  "post_i2c"
      [None; None; None; None; None; Some "sos_may_release"; None; None];

    Hashtbl.add annotations  "post_spi"
      [None; None; None; None; None; Some "sos_may_release"; None; None];

    Hashtbl.add annotations  "ker_msg_take_data"
      [Some "sos_claim"; None; None];

    Hashtbl.add annotations  "msg_duplicate"
      [Some "sos_claim"; None];

    Hashtbl.add annotations  "msg_create"
      [Some "sos_claim"];

    Hashtbl.add annotations  "handle_incoming_msg"
      [None; Some "sos_release"; None];

    Hashtbl.add annotations  "sched_msg_alloc"
      [None; Some "sos_release"];

    Hashtbl.add annotations  "mq_enqueue"
      [None; None; Some "sos_release"];

    Hashtbl.add annotations  "ker_cam_add"
      [None; None; Some "sos_release"];

    Hashtbl.add annotations  "sos_msg_dispatch"
      [None; Some "sos_release"];

    Hashtbl.add annotations  "pop_new_mod_op"
      [Some "sos_claim"; None];

    annotations
;;



let feature : featureDescr = {
  fd_name = "addAnnotations";
  fd_enabled = ref true;
  fd_description = "Add annotations to function prototypes.";
  fd_extraopt = 
    [
      ("--aa_dbg_verbose", Arg.Unit (fun _ -> dbg_verbose := true),
       "Enable more vebose debugging");
    ];
  fd_doit = 
    (function (f:file) -> 
      iterGlobals f (addAttributes (getAnnotations ())));
  fd_post_check = true;
}

