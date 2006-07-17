open Cil
open Pretty

module E = Errormsg
module MF = MustFlow
module IH = Inthash
module U = MemUtil

(* Runtime debugging flags. *)
let dbg_may_alias = ref false
let dbg_must_alias = ref false
let dbg_must_null = ref false
let dbg_takes_data = ref false
              
let mustAliasWrapper e target s =
 
    (* then check alias analysis. *)
    (* TODO: this assumes the analysis is "seeded" *) 
    (* TODO: The tansitive follow is can loop!  Ie. a = b; b = c; c = a;
     * All bad... *)
  match (U.getVarinfoFromExp e, U.getVarinfoFromExp target) with
      (Some v1, Some v2) ->

        let table =
          try 
            MF.getStmtState MF.DFM.stmtStartData s
          with
              _ -> 
                None
        in

        let nextVar v t = 
          try 
            Hashtbl.find t v
          with
              _ -> 
                if !dbg_must_alias then
                  ignore (printf 
                            "MUST ALIAS: No entery for var %s\n" v.vname);
                MF.End
        in

        let rec traverse v target t history =
          if (Util.equals v target) then (
            if !dbg_must_alias then
              ignore(printf 
                       "MUST ALIAS: Hit with Var %s equal to target %s\n" 
                       v.vname target.vname);
            true
          ) else (
            match (nextVar v t) with
                MF.Next vNew -> 
                  if (List.exists (fun vOld -> Util.equals vNew vOld) history) then (
                    if !dbg_must_alias then
                      ignore(printf 
                               "MUST ALIAS: Aborting... Found loop for var %s\n" 
                               v.vname);
                    false
                  ) else (
                    if !dbg_must_alias then
                      ignore(printf 
                               "MUST ALIAS: Transitive follow to var %s\n" 
                               vNew.vname);
                    traverse vNew target t (v::history)
                  )
              | MF.End -> 
                  if !dbg_must_alias then
                    ignore(printf 
                             "MUST ALIAS: Aborting... No follow for var %s\n" 
                             v.vname);
                  false
              | MF.Null ->
                  if !dbg_must_alias then
                    ignore(printf 
                             "MUST ALIAS: Aborting... Hit NULL pointer %s\n" 
                             v.vname);
                  false
          )
        in

          (match table with
               Some t -> 
                 if !dbg_must_alias then
                   ignore(printf 
                            "MUST ALIAS: Checking if var %s must alias %s\n" 
                            v1.vname v2.vname);
                 traverse v1 v2 t []
             | None -> 
                 if !dbg_must_alias then
                   ignore (printf 
                             "MUST ALIAS: Aborting... No lookup table for state %a\n" 
                             d_stmt s);
                 false
          )

    | _ ->
        if !dbg_must_alias then
          ignore(printf 
                   "MUST ALIAS: Failed to find one of either %a or %a\n" 
                   d_exp e d_exp target);
        false
        
               


let mustAliasNull e s =
 
  match U.getVarinfoFromExp e with
      Some v1 ->

        let table =
          try 
            MF.getStmtState MF.DFM.stmtStartData s
          with
              _ -> 
                None
        in

        let nextVar v t = 
          try 
            Hashtbl.find t v
          with
              _ -> 
                if !dbg_must_null then
                  ignore (printf 
                            "MUST NULL: No entery for var %s\n" v.vname);
                MF.End
        in

        let rec traverse v t history =
          match (nextVar v t) with
              MF.Next vNew -> 
                if (List.exists (fun vOld -> Util.equals vNew vOld) history) then (
                  if !dbg_must_null then
                    ignore(printf 
                             "MUST NULL: Aborting... Found loop for var %s\n" 
                             v.vname);
                  false
                ) else (
                  if !dbg_must_null then
                    ignore(printf 
                             "MUST NULL: Transitive follow to var %s\n" 
                             vNew.vname);
                  traverse vNew t (v::history)
                )
            
            | MF.End -> 
                if !dbg_must_null then
                  ignore(printf 
                           "MUST NULL: Aborting... No follow for var %s\n" 
                           v.vname);
                false
            | MF.Null ->
                if !dbg_must_null then
                  ignore(printf 
                           "MUST NULL: BINGO... Hit NULL pointer %s\n" 
                           v.vname);
                true
        in

          (match table with
               Some t -> 
                 if !dbg_must_null then
                   ignore(printf 
                            "MUST NULL: Checking if var %s must be null pointer\n" 
                            v1.vname);
                 traverse v1 t []
             | None -> 
                 if !dbg_must_null then
                   ignore (printf 
                             "MUST NULL: Aborting... No lookup table for state %a\n" 
                             d_stmt s);
                 false
          )

    | _ ->
        if !dbg_must_null then
          ignore(printf 
                   "MUST NULL: Failed to find var %a\n" 
                   d_exp e);
        false
        
               


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




