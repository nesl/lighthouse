open Cil
open Pretty

module E = Errormsg
module IH = Inthash
module U = MemUtil

(* Runtime debugging flags. *)
let dbg_may_alias = ref false
let dbg_takes_data = ref false
              

(* TODO: This is a little tricky.  What about members of a structure?  Is this
 * always correct?  Argh! *)                
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




