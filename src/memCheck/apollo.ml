open Cil
open Pretty

module IH = Inthash;;
module DF = Dataflow;;

module E = Errormsg;;
module IE = IsEquivalent;;
module MA = MayAliasWrapper;;
       
(* Runtime debugging *)
let dbg_apollo_s = ref false;;
let dbg_apollo_i = ref false;;
let dbg_apollo_c = ref false;;
let dbg_apollo_df = ref false;;

(* State maintained by the dataflow analysis.  This includes a set of stores and
 * a list of expressions refering to heap data that has not been stored. *)
type store_state = Empty | Full | Unknown | Error;;

type store_data = (store_state * exp);;
type heap_data = exp;;

type dataflow_state = (store_data list) * (heap_data list);;

(* Reference to the current statment *)
let currentStmt = ref (mkEmptyStmt ());;


(* Helper functions *)

(* State info types used to describe an expression as:
 * Heap -> Heap data
 * Stare -> Store data
 * Complex -> Combination of one or more heap and / or store datas
 * Other -> Nothing of interest such as a constant
 *)
type state_info = 
   Heap of exp 
  | Store of exp
  | Complex of exp
  | Other
;;


(* This function is used to extract state relevant information from an
 * expression.
 *)
let rec get_state_from_exp (e: exp): state_info =
  match e with
    | Lval lv when is_heap lv -> Heap (Lval (var lv))

    | Lval lv when is_store lv -> Store (Lval (var lv))
    
    | UnOp (_, e, _) -> mem_from_exp e

    | BinOp (_, e1, e2, _) -> 
        begin
          match (mem_from_exp e1, mem_from_exp e2) with
              (Other, tmp) -> tmp
            | (tmp, Other) -> tmp
            | _ -> Complex e
        end

    | CastE (_, e) -> mem_from_exp e

    | AddrOf lv
    | StartOf lv -> Other
                                    
    | Lval _                                    
    | Const _ 
    | SizeOf _ 
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _
      -> Other
;;

        

(****************************************)
(****************************************)
(* Data  flow implementation *)
(****************************************)
(****************************************)
module Apollo_Dataflow = struct

  (* Vital stats for this dataflow. *)
  let name = "apollo";;
  let debug = dbg_apollo_df;;
  type t = dataflow_state;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "%s" (
      let (stores, heaps) = state in
      let s = 
        List.fold_left 
          (fun s store -> 
             match store with
                 (Empty, e) ->
                   s ^ (sprint 70 (dprintf "Store %a in state Empty\n" d_exp e))
               | (Full, e) ->
                   s ^ (sprint 70 (dprintf "Store %a in state Full\n" d_exp e))
               | (Unknown, e) -> 
                   s ^ (sprint 70 (dprintf "Store %a in state Unknown\n" d_exp e))
          ) 
          "Stores:\n" 
          stores 
      in
      let s =
        List.fold_left 
          (fun s heap -> 
             s ^ (sprint 70 (dprintf "Heap referenced by expression %a\n" d_exp heap))
          ) 
          (s ^ "Heap Data:\n") 
          heaps 
      in
        s
    )
  ;;


  let computeFirstPredecessor (s: stmt) (state: t): t = state;;


  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) = 
    None
  ;;


  let doInstr (i: instr) (state: t): t DF.action = 
    match i with
       
        Set (lv, e, _) ->
          begin
            match get_state_from_exp e with
                
                Heap e when (is_store lv state) ->
                  (* "Transfer" heap data into the store. *)
                  let new_state = fill_store lv state in
                  let new_state = remove_heap e new_state in
                    DF.Done new_state

              | Heap e ->
                  (* Using heap data.  Tranfer depends on use_heap function. *)
                  let new_state = use_heap e state in
                    new_state
                      
              | Store e when (is_store lv state) ->
                  (* "Transfer" heap data from one store to another. *)
                  let new_state = fill_store lv state in
                  let new_state = empty_store e new_state in
                    DF.Done new_state
              
              | Store e ->
                  (* Using store data.  Transfer depends on use_store function. *)
                  let new_state = use_store e state in
                    DF.Done new_state
                      
              | Complex e ->
                  (* This is something that we do not currently handel. *)
                  E.s (E.bug "%s %s %a\n" 
                         "Apollo.Apollo_Dataflow.doInstr:" 
                         "Unable to update state given set with rval" 
                         d_exp e)
                  
              | Other ->
                  (* This set expression has no effect (and is not effected by)
                   * the current state *)
                  DF.Default
          end
      
      
      | Call (lvop, e, el, _) ->
          let alloced = get_alloced_formals e el in
          let freed = get_freed_formals e el in
          let return_alloc = returns_alloc e in


      | Call (lvop, e, el, _) ->
          test_access e el;
          get_alloc e el;
          get_free e el;
      | _ -> DF.Default
  ;;


  let doStmt (s: stmt) (state: t) =
    currentStmt := s;
    ignore (Pretty.printf "%a\n" pretty state);
    DF.SDone
  ;;

  let doGuard _ _ = 
    DF.GDefault
  ;;
            
  let filterStmt _ = 
    true
  ;;

end;;


module Track = DF.ForwardsDataFlow(Apollo_Dataflow);;



(****************************************)
(****************************************)
(* Interface Functions *)
(****************************************)
(****************************************)


let apollo_func (f: fundec) (state: dataflow_state): bool =

  IH.clear Apollo_Dataflow.stmtStartData;
  IH.add Apollo_Dataflow.stmtStartData (List.hd f.sbody.bstmts).sid  state;
  Track.compute [List.hd f.sbody.bstmts];
    
  true 
;;



