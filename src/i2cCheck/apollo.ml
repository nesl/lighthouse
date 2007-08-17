open Cil;;
open Pretty;;

open State;;

module IH = Inthash;;
module DF = Dataflow;;
module E = Errormsg;;

(* Runtime debugging *)
let dbg_i2c_s = ref false;;
let dbg_i2c_i = ref false;;
let dbg_i2c_c = ref false;;
let dbg_i2c_df = ref false;;
let dbg_i2c_g = ref false;;
let dbg_i2c = ref false;;
let strict = ref false;;

(* Reference to the current statment *)
let current_stmt = ref (mkEmptyStmt ());;

(* Reference to an error log *)
let error_log = ref "Error Log:\n";;

(* Reference to the file we are working with *)
let cil_file = ref dummyFile;;

(****************************************)
(****************************************)
(* Data  flow implementation *)
(****************************************)
(****************************************)
module I2C_Dataflow = struct

  (* Vital stats for this dataflow. *)
  let name = "i2c";;
  let debug = dbg_i2c_df;;
  type t = i2c_state;;

  (* Basic util functions to jumpstart dataflow. *)
  let stmtStartData: t IH.t = IH.create 17;;
  let copy (state: t) = state;;
  let pretty () (state: t) =
    dprintf "%s" (State.i2c_state_to_string state);;


  let computeFirstPredecessor (s: stmt) (state: t): t = state;;


  let combinePredecessors (s: stmt) ~(old: t) (new_state: t) = 

    if !dbg_i2c_c then (
      ignore (printf "Checking combine at %a\n" 
                d_loc (get_stmtLoc !current_stmt.skind));
      flush stdout;
    );

    let merged = match (new_state, old) with
        (Reserved, Reserved)
      | (Free, Free)
      | (Unknown, _) 
      | (_, Error) -> None

      | (Reserved, Unknown) -> Some Reserved
      | (Free, Unknown) -> Some Free

      | (Error, _)
      | (Reserved, Free)
      | (Free, Reserved) -> Some Error
    in
    
      if !dbg_i2c_c then (
        match merged with
            Some state -> ignore (printf "Combined to %a\n" pretty state);
          | None -> ignore (printf "Stayed the same at %a\n" pretty old);
        flush stdout;
      );
      merged
  ;;


  let doInstr (i: instr) (state: t): t DF.action = 

    if !dbg_i2c_i then (
      ignore (printf "Checking instruction at %a\n" d_loc (get_instrLoc i));
      flush stdout;
    );

    match i with
        
        Call (lvop, Lval((Var v), NoOffset), el, _) ->

          if !dbg_i2c_i then (
            ignore (printf "State coming into called function %s is:\n%a\n" 
                      v.vname pretty state);
            flush stdout;
          );

          (* Verify that we are in a state that is valid for the function call
           *)
          let proper_pre = verify_state_with_pre v.vname state in

          (* Place holder for checking for errors.  The function
           * verify_state_with_pre currently prints a debugging statement for
           * us. 
           *)
          let _ = if not proper_pre then () in

          (* Update state to reflect the effects of the function call *)
          let new_state = update_state_with_post v.vname state in

            if !dbg_i2c_i then (
              ignore (printf "State leaving called function %s is:\n%a\n" 
                        v.vname pretty state);
              flush stdout;
            );


            if (compare new_state state) = 0 then (
              DF.Default
            ) else (
              DF.Done new_state
            )

      | Call _ ->
          E.bug "Have not implemented this form of call at %a"
            d_loc (get_stmtLoc !current_stmt.skind);
          DF.Default


      | _ -> DF.Default
  ;;


  let doStmt (s: stmt) (state: t) =
    current_stmt := s;

    if !dbg_i2c_s then (
      ignore (printf "Checking statement at %a\n" d_loc (get_stmtLoc !current_stmt.skind));
      flush stdout;
    );

    DF.SDefault
  ;;


  (* TODO: Add utility to track and then check the return value from calls to
   * I2C related functions. *)
  let doGuard (e: exp) (state: t) = 

    if !dbg_i2c_g then (
      ignore (printf "Checking guard at %a\n" d_loc (get_stmtLoc !current_stmt.skind));
      flush stdout;
    );

    DF.GDefault
  ;;


  let filterStmt _ = true ;;

end;;


module Track = DF.ForwardsDataFlow(I2C_Dataflow);;

(****************************************)
(****************************************)
(* Interface Functions *)
(****************************************)
(****************************************)

let i2c_func (f: fundec) (cfile: file) : unit = 

  cil_file := cfile;

  let state = 
    State.update_state_with_pre f.svar.vname Unknown
  in

  let _ = 
    if !dbg_i2c then (   
      ignore (printf "State coming into function %s is:\n%a\n" 
                f.svar.vname I2C_Dataflow.pretty state);
      flush stdout;
    )
  in

  let _ = 
    (* Run dataflow for function *)
    IH.clear I2C_Dataflow.stmtStartData;
    IH.add I2C_Dataflow.stmtStartData (List.hd f.sbody.bstmts).sid  state;
    Track.compute [List.hd f.sbody.bstmts];
    ()
  in

  let return_stmts = MemUtil.get_return_statements f in

    (* For each return point verify that the post conditions required by the
     * function are satisfied by the state *)
    List.iter 
      (fun s -> 
         try
           let return = (IH.find I2C_Dataflow.stmtStartData s.sid) in 

           let _ = 
             if !dbg_i2c then (   
               ignore (printf "State leaving function %s at %a is:\n%a\n" 
                         f.svar.vname 
                         d_loc (get_stmtLoc s.skind) 
                         I2C_Dataflow.pretty return);
               flush stdout;
             )
           in


           let safe_return = verify_state_with_post f.svar.vname return in

             if not safe_return then ( 
               (*
                E.error 
                "Return at %a fails to satisfy post- conditions for function %s"
                d_loc (get_stmtLoc s.skind) f.svar.vname;
                *)
               ()
             );
         with Not_found -> 
           (*
            E.error "Unable to find state for return statement %d (%a)" 
            s.sid
            d_loc (get_stmtLoc s.skind) ;
            *)
           ()

      )
      return_stmts;

    ()
;;

let i2c_func_simple (f: fundec) (cfile: file): unit =
  i2c_func f cfile
;;


let i2c_func_fsm (f: fundec) (cfile: file): unit =
  i2c_func f cfile
;;


