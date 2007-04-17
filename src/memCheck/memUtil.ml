open Cil
open Pretty

module E = Errormsg
             
let verbose = ref false;;

(* Custom helper function to obtain the "parents" of an expression, where a
 * parent is a sub-expression (field or memory offset has been droped).
 * 
 * Note that we do NOT transition from a Mem to an Lval.  This would be like
 * saying that "a" is a parent of "a->b".  The correct thing to say is that "*a"
 * is a parent of "a->b".
 *
 *)
let rec get_parents_of (parent: exp) : exp list = 

  let parent = stripCasts parent in
  
  match parent with   

    (* TODO: Note that &a is a parent of a.  In light of this, is this the
     * correct thing to do...
     *)      
    | AddrOf (Mem _, _)
    | StartOf (Mem _, _) 
    | AddrOf (Var _, _)
    | StartOf (Var _, _) ->
        []
        
    | Lval (Mem e, NoOffset) ->
        (stripCasts e) :: (get_parents_of e)

    | Lval (Var v, _) ->
        (Lval (var v))::[AddrOf (var v)]
    
    | Lval (Mem e, _) ->
        (stripCasts (Lval (Mem e, NoOffset))) :: (get_parents_of (Lval (Mem e, NoOffset)))

    | BinOp (PlusPI, e, _, _)
    | BinOp (IndexPI, e, _, _)
    | BinOp (MinusPI, e, _, _) ->
        (stripCasts e) :: (get_parents_of e)

    | BinOp (PlusA, e, c, _)
    | BinOp (MinusA, e, c, _) when (isConstant c) ->
        (stripCasts e) :: (get_parents_of e)

    | BinOp (PlusA, c, e, _)
    | BinOp (MinusA, c, e, _) when (isConstant c) ->
        (stripCasts e) :: (get_parents_of e)

    | BinOp (PlusA, e1, e2, _) 
    | BinOp (MinusA, e1, e2, _)
    | BinOp (MinusPP, e1, e2, _) ->
        (stripCasts e1) :: (stripCasts e2) :: (get_parents_of e1) @ (get_parents_of e2)

    | UnOp (_, e, _) ->
        (stripCasts e) :: (get_parents_of e)

    | CastE (_, e) ->
        E.s (E.bug "MemUtil.get_parents_of: Casts should already be striped")
    
    | BinOp _
    | Const _ 
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _ ->
        []
;;


(* Check to see if sub is a parent of e *)
let rec is_parent_of (parent:exp) (child:exp) : bool =

  let parents_of = child::(get_parents_of child) in
  
  List.exists 
    (fun child -> Util.equals (stripCasts parent) (stripCasts child)) 
    parents_of

;;
                      

(* Generates a list of return statements in a function.  Assumes that
 * computeCFGInfo or a simaliar function has been called.
 *)
let get_return_statements (f: fundec) : stmt list =
  List.filter 
    (fun s -> match s.skind with Return _ -> true | _ -> false) 
    f.sallstmts
;;


(*
let get_return_exp_with_attribute (i:instr) (attr_name:string): exp list =

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
              
              return_exp
          )
    
    (* Catch for functions (such as function pointers) that we are unable to
     * handle. *)
    | Call (_, _, _, _) ->
        if !verbose then (
          ignore (E.warn "MemUtil.get_formal_exps_with_attribute:"); 
          ignore (E.warn "Unable to understand function call %a.  Skipping.\n" d_instr i);
        );
        []
       
    | _ -> []
;;
 *)


(* Takes an instruction and the name of an attribute.  This function checks to
 * so if the instruction is a Call and returns the empty list if it is not.  If
 * the instruction is a Call, then the function prototype is used to find if any
 * of the formals or return value of the attribute named [s] set.  For each such
 * formal or return value, the corresponding expressions from the instruction
 * are returned. *)

(*
let get_formal_exps_with_attribute (i:instr) (attr_name:string): exp list =

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

              formal_exps
          )
    
    (* Catch for functions (such as function pointers) that we are unable to
     * handle. *)
    | Call (_, _, _, _) ->
        if !verbose then (
          ignore (E.warn "MemUtil.get_formal_exps_with_attribute:"); 
          ignore (E.warn "Unable to understand function call %a.  Skipping.\n" d_instr i);
        );
        []
       
    | _ -> []
;;
 *)

(* Return list of expressions in function call that have the "sos_claim"
 * attribute set *)
(*
let get_claim (i:instr): exp list =
  (get_return_exp_with_attribute i "sos_claim") @ (get_formal_exps_with_attribute i "sos_claim")
;;
 *)

(*
let get_claim_formals (i:instr): exp list =
  (get_formal_exps_with_attribute i "sos_claim")
;;
 *)

(*
let returns_alloc (i: instr): bool = 
  (List.length (get_return_exp_with_attribute i "sos_claim")) > 0
;;
 *)


(* Given an instruction return a list of expressions corresponding to
 * foramal paramaters that have the 'sos_release' flag set, or that have the
 * 'sos_may_release' flag set and pass a may release test. *)
(*
let get_released (i:instr): exp list =

  let must_release = get_formal_exps_with_attribute i "sos_release" in
 

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
          get_formal_exps_with_attribute i "sos_may_release"
        else []

    | Call (_, Lval((Var vi), NoOffset), formal_list, _) when (List.length formal_list) > 0 ->
        if check_release_flag (List.nth formal_list ((List.length formal_list) -1)) then
          get_formal_exps_with_attribute i "sos_may_release"
        else []
    
    | Call (_, _, _, _) ->
        if (List.length (get_formal_exps_with_attribute i "sos_may_release") > 0) then
          E.s (E.error "MemCheck.get_released: Incorrect set of sos_may_release in %a\n"
                d_instr i)
        else []

    | _ -> []
             
  in

    may_release @ must_release
;;
 *)

(* Return a list of functions name and formal number (zero for return value)
 * pairs for return / foramls that have the attr_name attribute set. *)
let get_attribute_funcs (attr_name: string) (f: file) : (string * int) list =

  foldGlobals 
    f 
    (fun allocs g -> match g with
         GFun (f, _) ->
           let (return_type, formals_op, is_vararg, attributes) = 
             splitFunctionType f.svar.vtype
           in

           let rec alloc_formals formals (counter: int) : (string * int) list= 
             match formals with
                 [] -> []
               | (_, _, attrs)::tl when (hasAttribute attr_name attrs) ->
                   (f.svar.vname, counter)::(alloc_formals tl (counter + 1))
               | hd::tl -> alloc_formals tl (counter + 1)
           in

           let formals = match formals_op with
             | Some f -> f 
             | None -> []
           in

             if (hasAttribute attr_name (typeAttrs return_type)) then 
               (f.svar.vname, 0) :: (alloc_formals formals 1) @ allocs
             else 
               (alloc_formals formals 1) @ (allocs)
       | _ -> allocs
    )
    []
;;


let get_alloc_funcs (f: file) : (string * int) list = 
  get_attribute_funcs "sos_claim" f
;;

    
let get_free_funcs (f: file) : (string * int) list = 
  get_attribute_funcs "sos_release" f
;;

    

(* Take a list as input.  Return a "sorted" version of the list with only uniq
 * members *)
let sort_and_uniq (l:'a list) : 'a list =
  let rec uniq l = match l with
      [] -> []
    | hd::[] -> [hd]
    | hd::next::rest ->
        if ((compare hd next) = 0) then
          uniq (hd::rest)
        else
          hd::(uniq (next::rest))
  in
    uniq (List.sort compare l)
;;

