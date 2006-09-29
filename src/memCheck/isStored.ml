open Cil
open Pretty

module IE = IsEquivalent;;
module E = Errormsg;;
module U = MemUtil;;
             
let dbg_is_store = ref false;;
             

(************************************************************)
(* Interface functions to the dataflow *)
(************************************************************)

let get_return_statements (f: fundec) : stmt list =
  List.filter 
    (fun s -> match s.skind with Return _ -> true | _ -> false) 
    f.sbody.bstmts
;;


let set_claim_on_return (f: fundec) : bool =
  match f.svar.vtype with
      TFun (t, _, _, _) when (hasAttribute "sos_claim" (typeAttrs t)) -> true
    | _ -> false
;;


(* Check to see if all dynamic memory created in function f is stored *)
let not_stored_exps (f: fundec) (stores: exp list) : exp list =

  (* Return true if expression list "is equivalent" to a store or stored in
   * another heap variable *)
  (* TODO:  What about circular data structures? Ie.
   *   a->next = b;
   *   b->next = c;
   *   c->next = a;
   *)
  let rec contains_one_store (equiv_set: exp list) (sid: int) (return : exp option) : bool =
    
    if !dbg_is_store then (
      ignore (printf "*** Examining equiv set for a store:\n");
      List.iter
        (fun e -> ignore (printf "    %a\n" d_exp e))
        equiv_set;
      flush stdout;
    );
    
    let direct_store = 
      List.exists
        (fun e -> List.exists (fun store -> IE.is_equiv e store sid) stores)
        equiv_set
    in
      
    let is_heap_field (e: exp) : bool =
      match (stripCasts e) with 
          Lval (Var v, Field _) 
        | Lval (Var v, Index _) ->
            contains_one_store (IE.get_equiv_set (Lval (var v)) sid) sid return
        | Lval (Mem e, _) -> 
            contains_one_store (IE.get_equiv_set e sid) sid return

        | BinOp (IndexPI, e, _, _)
        | BinOp (MinusPI, e, _, _) ->
            contains_one_store (IE.get_equiv_set e sid) sid return

        | BinOp (PlusA, e, c, _)
        | BinOp (MinusA, e, c, _) when (isConstant c) ->
            contains_one_store (IE.get_equiv_set e sid) sid return

        | BinOp (PlusA, c, e, _)
        | BinOp (MinusA, c, e, _) when (isConstant c) ->
            contains_one_store (IE.get_equiv_set e sid) sid return

        | _ -> false
    in

    let return =
      match return with
          Some r ->
            (set_claim_on_return f) && 
            (List.exists (fun e -> IE.is_equiv r e sid) equiv_set)
        | None ->
            false
    in
    
    let indirect_store = List.exists is_heap_field equiv_set 
    in
    
      direct_store || return || indirect_store
  in


  (* Return any heap expressions within a list *)
  let get_heaps (el: exp list) : exp list = 
    List.filter
      (fun e -> match e with
           Lval (Var v, NoOffset) ->
             (Str.string_match (Str.regexp "__heap") v.vname 0)
         | _ -> false
      )
      el
  in

  
  let bad_stores = 
    List.fold_left
      (fun bad_stores s -> 
        
         let ell = IE.get_equiv_sets s.sid in
         
         List.fold_left
           (fun bad_stores el -> 
              let heaps = get_heaps el in

                (* If there are no heap expressions, then all is safe *)
                if (List.length heaps < 1) then
                  bad_stores
                
                (* If two heap expressions must be equivalent, then something is
                 * wrong *)
                else if (List.length heaps > 1) then
                  heaps @ bad_stores

                (* Otherwise, check that the heap expression is either stored or
                * returned by the function*)
                else (
                  
                  if !dbg_is_store then (
                    ignore (printf "\n\nLooking at heap data %a\n" 
                              d_exp (List.nth heaps 0));
                    flush stdout;
                  );
                  
                  match s.skind with 
                      Return (Some return, _) ->
                        if contains_one_store el s.sid (Some return) then 
                          bad_stores
                        else (heaps @ bad_stores)
                    | _ ->
                        if contains_one_store el s.sid None then bad_stores
                        else (heaps @ bad_stores)
                )
           )
           bad_stores
           ell
      )
      []
      (get_return_statements f)
  in
    
  let bad_stores = U.sort_and_uniq bad_stores in

    
    if !dbg_is_store then (
      if List.length bad_stores = 0 then (
        ignore (printf "IsStored.not_stored_exps: Function %s safely stores data\n" f.svar.vname);
      ) else (
        ignore (printf "IsStored.not_stored_exps: Function %s fails to store:\n" f.svar.vname);
        List.iter (fun e -> ignore (printf "    %a\n" d_exp e)) bad_stores;
      )
    );

    bad_stores
;;
          
