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


