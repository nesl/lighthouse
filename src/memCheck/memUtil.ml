open Cil
open Pretty

module E = Errormsg
             


(* Check to see if sub is a subexpression of e *)
let rec is_subexpression_of (sub:exp) (e:exp) : bool =
  if (Util.equals (stripCasts sub) (stripCasts e)) then (
    true
  ) else (
    match e with
        Lval (Mem e, _) 
      | CastE (_, e)
      | AddrOf (Mem e, _)
      | StartOf (Mem e, _)
      | UnOp (_, e, _) ->
          is_subexpression_of sub e

      | BinOp (_, e1, e2, _) ->
          (is_subexpression_of sub e1) || (is_subexpression_of sub e2)

      | _ -> false
  )

;;
                      
                      
(* Takes an instruction and the name of an attribute.  This function checks to
 * so if the instruction is a Call and returns the empty list if it is not.  If
 * the instruction is a Call, then the function prototype is used to find if any
 * of the formals or return value of the attribute named [s] set.  For each such
 * formal or return value, the corresponding expressions from the instruction
 * are returned. *)
let get_fun_exps_with_attribute (i:instr) (attr_name:string): exp list =

  match i with
  
      Call (lop, Lval((Var vi), NoOffset), formal_list, _) ->
      
        let (return_type, formals_op, is_vararg, attributes) = 
          splitFunctionType vi.vtype
        in

          (* For simplicity skip over variable argument functions. *)
          if is_vararg then (
            ignore (E.warn "MemUtil.get_own: Skipping variable argument function\n");
            []
          ) else (

            (* If storing the return value and the function prototype sets
             * attribute for the return value, add it to the expression list.
             *)
            let return_exp = match lop with
                Some lv when (hasAttribute attr_name (typeAttrs return_type)) ->
                  [Lval lv] 
              | _ -> []
            in


            (* TODO: Is formal_attrs the same as (typeAttrs formal_type) in the
             * code below? *)
            let formal_exps = match formals_op with
                Some str_type_attr_list ->
                  List.fold_left2 
                    (fun has_attribute (_, formal_type, formal_attrs) formal -> 
                       if (hasAttribute attr_name formal_attrs) then
                         formal::has_attribute
                       else has_attribute
                    )
                    []
                    str_type_attr_list
                    formal_list
              | None -> []
            in

              return_exp @ formal_exps
          )
    
    (* Catch for functions (such as function pointers) that we are unable to
     * handle. *)
    | Call (_, _, _, _) ->
        ignore (E.warn "MemUtil.get_gun_exps_with_attribute:"); 
        ignore (E.warn "Unable to understand function call %a.  Skipping.\n" d_instr i);
        []
       
    | _ -> []
;;


(* Return list of expressions in function call that have the "sos_claim"
 * attribute set *)
let get_claim (i:instr): exp list =
  get_fun_exps_with_attribute i "sos_claim"
;;


(* Given an instruction return a list of expressions corresponding to
 * foramal paramaters that have the 'sos_release' flag set, or that have the
 * 'sos_may_release' flag set and pass a may release test. *)
let get_released (i:instr): exp list =

  let must_release = get_fun_exps_with_attribute i "sos_release" in
 

  (* ======================================== *) 
  (* The following section is VERY specific to the optional releases used within
   * SOS.  Blah. *)

  (* Helper function to see if an exprssion sets the the release flag of value 0x04 *) 
  (* NOTE: This assumes that the flag used in post calls is a compile time
   * constant. *)
  let check_release_flag (flag: exp): bool = 
    match flag with 
        Const(CInt64(i, _, _)) 
          when (Int64.logand i (Int64.of_int 0x04) > (Int64.of_int 0)) -> true 
      | _ -> false 
  in

  let may_release = match i with 

      Call (_, Lval((Var vi), NoOffset), formal_list, _)
        when (vi.vname = "post_net" || 
              vi.vname = "post_link" || 
              vi.vname = "post_uart" ||
              vi.vname = "post_i2c" ||
              vi.vname = "post_spi" ||
              vi.vname = "post_auto") ->
        if check_release_flag (List.nth formal_list 5) then
          get_fun_exps_with_attribute i "sos_may_release"
        else []

    | Call (_, Lval((Var vi), NoOffset), formal_list, _) ->
        if check_release_flag (List.nth formal_list ((List.length formal_list) -1)) then
          get_fun_exps_with_attribute i "sos_may_release"
        else []
    
    | Call (_, _, _, _) ->
        if (List.length (get_fun_exps_with_attribute i "sos_may_release") > 0) then
          E.s (E.error "MemCheck.get_released: Incorrect set of sos_may_release in %a\n"
                d_instr i)
        else []

    | _ -> []
             
  in

    may_release @ must_release
;;


