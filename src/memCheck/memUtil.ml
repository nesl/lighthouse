open Cil
open Pretty

module E = Errormsg
module IH = Inthash
              
(* Run time debugging flags *)
let dbg_mem_util = ref false

(* Runtime debugging flags. *)
let dbg_may_alias = ref false
let dbg_takes_data = ref false
              
let mayAliasWrapper e target =
  
  try 
    let alias = Ptranal.may_alias e target in
      
      if (!dbg_may_alias && alias) then 
        ignore 
          (printf 
             "MAY ALIAS: Expression %a may alias target expression %a\n" 
             d_exp e d_exp target)
      else if (!dbg_may_alias && (not alias)) then
        ignore 
          (printf 
             "MAY ALIAS: Expression %a must not alias target expression %a\n" 
             d_exp e d_exp target);
      
      alias
  
  with
      Not_found -> 
        if !dbg_may_alias then
          ignore 
            (printf 
               "MAY ALIAS: Expression %a not found in alias analysis check against %a\n" 
               d_exp e d_exp target);
        false
    | _ -> ignore (E.bug "Strange error alias check\n"); false
;;




let rec getVarinfoFromLval (l:lval) : (varinfo option) =
      match l with
          (Var v, _) -> Some v
        | (Mem e, _) -> getVarinfoFromExp e

and getVarinfoFromExp (e:exp) : (varinfo option) =
      match e with

        | Lval l
        | AddrOf l
        | StartOf l -> (getVarinfoFromLval l)

        (* Watch out for the NULL pointer... *)
        | Const c -> None

        (* Why not drop all type casts here... *)
                       (*
        | CastE (TPtr (TFun (_, _, _, _), a), eTmp) ->
            None

        | CastE (TArray (t, _, a), eTmp) ->
            getVarinfoFromExp eTmp

        | CastE (TPtr (t, a), eTmp) ->
            getVarinfoFromExp eTmp

        | CastE (TInt (IUInt, _), eTmp) ->              
            getVarinfoFromExp eTmp
                        *)
        
        | CastE (_, eTmp) ->              
            getVarinfoFromExp eTmp
              
        (* I am pretty sure that this will simply fold pointer indexes
         * into the base lval *)
        | UnOp (u, e, t) when isPointerType (t) || isArrayType (t) -> getVarinfoFromExp e
        | BinOp (b, e1, e2, t) when isPointerType(t) || isArrayType(t) ->
            begin
              match (getVarinfoFromExp e1, getVarinfoFromExp e2) with
                  (Some v, None)
                | (None, Some v) -> Some v
                | _ ->
                    if !dbg_mem_util then
                      ignore (E.warn "Can not get lval from BinOp %a\n" d_exp e);
                    None
            end

        (* These types return ints rather than referenecs *)
        | SizeOf t -> None
        | SizeOfE e -> None
        | SizeOfStr s -> None
        | AlignOf t -> None
        | AlignOfE e -> None
        | _ -> None



                       
                
(* Given an instruction return a list of expressions corresponding to
 * foramal paramaters that have the 'sos_claim' flag set. *)

(* TODO: If the function does not store the return value, but the return value
 * must be stored, this function will overlook the memory leak. *)
let getOwn (i:instr): exp list =
    
  match i with 

      Call (lop, Lval((Var vi), _), elist, _) 
       when ((String.compare vi.vname "printf") != 0) && ((String.compare vi.vname "snprintf") != 0) ->

        let mustExp = 
          match vi.vtype with
              TFun (t, Some alists, _, _) ->

                (* Grab expressions that must be claimed. *)
                let ael = match lop with
                    Some lv -> [(typeAttrs t, Lval lv)] 
                  | None -> []
                in

                let ael = 
                  try
                    List.fold_left 
                      (fun al ((_, _, a), e) -> (a, e)::ael) 
                      ael 
                      (List.combine alists elist)
                  with
                      Invalid_argument _ -> 
                        ignore (E.warn "Incorrect number of paramaters passed to function %s\n" vi.vname);
                        ael
                    | _ -> 
                        E.s (E.bug "Not too sure what went wrong with the list combine...")
                in

                (* Find those expressions with the "sos_claim" attribute set
                 *)
                let must = 
                  List.filter (fun (a, e) -> hasAttribute "sos_claim" a) ael
                in

                let (_, mustExp) = List.split must
                in
                  mustExp
                                 
            | TFun (t, None, _, _) ->

                (* Grab expressions that must be claimed. *)
                let ael = match lop with
                    Some lv -> [(typeAttrs t, Lval lv)] 
                  | None -> []
                in

                (* Find those expressions with the "sos_claim" attribute set
                 *)
                let must = 
                  List.filter (fun (a, e) -> hasAttribute "sos_claim" a) ael
                in

                let (_, mustExp) = List.split must
                in
                  mustExp
                                 
            | _ -> 
                []
        in
          mustExp
    
    | _ -> []

    
(* Given an instruction return a list of expressions corresponding to
 * foramal paramaters that have the 'sos_release' flag set. *)
let get_released (i:instr): exp list =

  let release = Int64.of_int 0x04 in

  
  (* Assume that expression e is a flag.  Return true if the bit set in
   * by f is set in the expression. *)
  let check_flag_ (e:exp) (f:int64): bool = 
    match e with 
        Const(CInt64(i, _, _)) 
          when ((Int64.logand i f) > (Int64.of_int 0)) 
        -> true 
      | _ -> false 
  in


  match i with 
      Call (_, Lval((Var vi), _), elist, _) 
when (String.compare vi.vname "printf") != 0 && (String.compare vi.vname "snprintf") != 0 ->

        begin
          match vi.vtype with
              TFun (_, Some alists, _, _) ->
               
                (* Grab expressions that must be released. *)
                let tmp = 
                  try
                    List.combine alists elist 
                  with
                      Invalid_argument _ -> 
                        ignore (E.warn "Incorrect number of paramaters passed to function %s\n" vi.vname);
                        []
                    | _ -> 
                        E.s (E.bug "Not too sure what went wrong with the list combine...")
                in

                let tmp = List.filter (fun ((_, _, a), e) -> 
                                         hasAttribute "sos_release" a) tmp in
                let (_, rlist) = List.split tmp in
                let must = rlist in

                (* Grab expressions that may be released and have a release
                 * flag set. Assume that the flag is the last parameter of
                 * the expression and is applied to all formal params with
                 * the 'sos_may_release' attribute set. Damn.  No go because
                 * of post_net.  Guess I need to do this by hand. *)
                let tmp = 
                  try
                    List.combine alists elist 
                  with
                      Invalid_argument _ -> 
                        ignore (E.warn "Incorrect number of paramaters passed to function %s\n" vi.vname);
                        []
                    | _ -> 
                        E.s (E.bug "Not too sure what went wrong with the list combine...")
                in
                  
                let tmp = 
                  List.filter 
                    (fun ((_, _, a), e) -> 
                       hasAttribute "sos_may_release" a) 
                    tmp 
                in
                let (_, rlist) = List.split tmp in
                let length = List.length rlist in
                let may = 
                  if 
                    (length > 0) &&
                    (
                      if (vi.vname = "post_net" 
                            || vi.vname = "post_link" 
                            || vi.vname = "post_uart" 
                            || vi.vname = "post_i2c" 
                            ||vi.vname = "post_spi"
                            || vi.vname = "post_auto") 
                      then
                        (check_flag_ (List.nth elist 5) release)
                      else
                        (check_flag_ (List.nth elist ((List.length elist) -1)) release) 
                    )
                  then rlist
                  else [] 
                in
                  must @ may
            | _ -> 
                []
        end
    | _ -> []


