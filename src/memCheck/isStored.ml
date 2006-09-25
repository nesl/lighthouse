open Cil
open Pretty

module IE = IsEquivalent
module E = Errormsg

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
  let contains_one_store (equiv_set: exp list) (sid: int) : bool =
    
    let direct_store = 
      List.exists
        (fun e -> List.exists (fun store -> IE.is_equiv e store sid) stores)
        equiv_set
    in
      
    let indirect_store =
      false
    in
      direct_store || indirect_store
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
                  match s.skind with 
                      Return (Some return, _) ->
                        if contains_one_store el s.sid then 
                          bad_stores
                        else if ((set_claim_on_return f) && 
                                 (IE.is_equiv return (List.nth heaps 0) s.sid)) then
                          bad_stores
                        else (heaps @ bad_stores)
                    | _ ->
                        if contains_one_store el s.sid then bad_stores
                        else (heaps @ bad_stores)
                )
           )
           bad_stores
           ell
      )
      []
      (get_return_statements f)
  in
    
  let rec uniq el = match el with
      [] -> []
    | hd::[] -> [hd]
    | hd::next::rest ->
        if (Util.equals hd next) then
          uniq (hd::rest)
        else
          hd::(uniq (next::rest))
  in

  let bad_stores = uniq (List.sort compare bad_stores) in

    
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
          
