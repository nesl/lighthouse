open Cil
open Pretty

module E = Errormsg
             
let verbose = ref false;;

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


