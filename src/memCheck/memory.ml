open Pretty
open Cil

open Simplemem
open Simplify
open MakeOneCFG

module IH = Inthash
module DF = IsDead
module OF = IsStored
module FF = CallerAllocatesLval
module RF = CallerAllocatesReturn
module U = MemUtil
module E = Errormsg
module IE = IsEquivalent             

(* State describing what happens to data from an "interesting" point in the
 * program.  Dead implies that the data is treated as dead on ALL paths from
 * that point forward.  Store implies that the data is stored exactly once on
 * EACH path from that point forward.  Any other state is an Error state.
 *)
type flowState = Error | Loops | Dead | Store
  
(* Run time debugging flags. *)
let dbg_free_exp = ref false
let dbg_alloc_exp = ref false
let dbg_alloc_stores = ref false
let dbg_ptr_arith = ref false
                         

(* Determine if allocated data is stored into a store. *)
let allocDataTreatment alloced s iop = 
  OF.targets := alloced;
  
  if (List.length alloced > 1) then
    ignore(E.bug "Analysis unable to handle statement allocing multiple expressions\n");
  
  if !dbg_alloc_exp then
    (
      ignore(printf "ALLOC EXP: Statement %a allocates:\n" d_stmt s);
      List.iter 
        (fun e -> ignore (printf "ALLOC EXP:  %a\n" d_exp e)) 
        !OF.targets;
    );


  IH.clear OF.DFO.stmtStartData;

  (*List.iter (fun s -> IH.add OF.DFO.stmtStartData s.sid OF.MustTake) s.succs;*)
  (*OF.Track.compute s.succs;*)
 
  begin
    match iop with 
        Some i ->
          (* Note is instruction saves data into a store *)  
          let isStore = 
            match i with
                Set (lv, _, _) 
              | Call (Some lv, _, _, _) ->
                  List.exists 
                    (fun store -> 
                       IE.is_equiv (Lval lv) store s.sid)
                    !OF.stores
              | _ -> false
          in
              
            if (isStore) then (
              IH.add OF.DFO.stmtStartData s.sid OF.Taken;
            ) else (
              IH.add OF.DFO.stmtStartData s.sid OF.MustTake;
            );
      | None ->
          IH.add OF.DFO.stmtStartData s.sid OF.MustTake;
  end;
  
  OF.Track.compute [s];
     
  (* Track the tuple (seen error, been taken) *)
  
  let (error, taken) = 
    IH.fold
      (fun sid t (error, taken) -> 
         match t with   
           | OF.ReturnTaken 
             -> 
               (error, true)
           
           | OF.MustTake 
           | OF.Taken 
           | OF.Null 
             -> 
               (error, taken)
           
           | OF.IfNull 
           | OF.Error 
             -> 
               (true, taken)
      )
      OF.DFO.stmtStartData
      (false, false)
  in

    if (not error && taken) then Store else Error
  

(* CIL visitor to verify that a strict "exactly one owner" memory model is
* followed in a program. *)
class memoryVisitor = object
  inherit nopCilVisitor

  val funStartStmt = ref (mkEmptyStmt ());
  val currentStmt = ref (mkEmptyStmt ());

                    
  method vfunc (f:fundec) = 
    funStartStmt := List.hd f.sbody.bstmts;
    
    (* Flow analysis for own needs to know the current function *)
    OF.currentFunc := Some f;
 
    (* Update the must alias analysis for this function *)
    IE.generate_equiv f;

    List.iter
      ( fun vi ->
          
          (* If formal var has store attribute set we can add it as a valid
           * store. *)
          if hasAttribute "sos_store" vi.vattr then (
            if (!dbg_alloc_stores) then 
              ignore (printf "ALLOC STORES: Store foraml var: %s\n" vi.vname);

            (* Add formal prameteres that release data to caller into stores
             * used by the ownFlow analysis. *)
            (* TODO: Technically, all of these should be saved up and appended
             * to the store AFTER we look at all formal parameters!  This
             * prevents us from using this formal parameter as a store for
             * another formal parameter. *)
            OF.stores := (Lval (var vi))::(!OF.stores);

          );
          
         

          (* If formal var has release attribute set, then this function must
           * store or release that formal variable. *) 
          if (hasAttribute "sos_release" vi.vattr) && (not (f.svar.vname = "ker_free")) then (
            if !dbg_alloc_exp then
              ignore (printf "ALLOC EXP: Function %s has formal var %s with release attribute\n" 
                        f.svar.vname vi.vname);

            let alloced = [Lval (var vi)] in
              match (allocDataTreatment alloced (List.hd f.sbody.bstmts) None) with
                  Store -> ()
                | _ -> 
                    ignore (E.warn 
                              "Formal var %s must be stored or released in function %s\n" 
                              vi.vname f.svar.vname);
          
          );


          (* If formal var has claim attribute set, then this function must
           * store or release that formal variable. Also, we need to add the var
           * to our list of "stores". *) 

          if hasAttribute "sos_claim" vi.vattr then (
            if !dbg_free_exp then
              ignore (printf "FREE EXP: Function %s has formal var %s with claim attribute\n" 
                        f.svar.vname vi.vname);
            if (!dbg_alloc_stores) then 
              ignore (printf "ALLOC STORES: Store foraml var: %s\n" vi.vname);

            (* Add formal prameteres that release data to caller into stores
             * used by the ownFlow analysis. *)
            (* TODO: Technically, all of these should be saved up and appended
             * to the store AFTER we look at all formal parameters!  This
             * prevents us from using this formal parameter as a store for
             * another formal parameter. *)
            OF.stores := (Lval (var vi))::(!OF.stores);

            (* Check that this reference is filled in before function returns. *)
            if not (FF.lval_is_allocated vi f) then
              ignore (E.warn 
                        "Formal var %s must be referenced to memory before end of function %s\n" 
                        vi.vname f.svar.vname);
          );
          ()
      )
      f.sformals;
    
    (* Use a special case to monitor for attributes on return values. *)
    (* TODO: Merge this into the code above. *)
    begin
      match f.svar.vtype with
          TFun (t, _, _, _) when 
            (
              (hasAttribute "sos_claim" (typeAttrs t)) && 
              (not (f.svar.vname = "ker_msg_take_data")) &&
              (not (f.svar.vname = "ker_malloc"))
            ) ->
            begin          

              if !dbg_free_exp then
                ignore (printf "FREE EXP: Function %s returns value with claim attribute\n" 
                          f.svar.vname);

              (* Check that this reference is filled in before function returns. *)
              IH.clear RF.DFR.stmtStartData;
              IH.add RF.DFR.stmtStartData (List.hd f.sbody.bstmts).sid RF.Empty;
              RF.Track.compute [List.hd f.sbody.bstmts];


              let (error, full) = 
                IH.fold
                  (fun sid t (error, taken) -> 
                     match t with   
                         RF.Empty -> (error, taken)
                       | RF.Full -> (error, true)
                       | RF.Error -> (true, taken)
                  )
                  RF.DFR.stmtStartData
                  (false, false)
              in

                if (not error && full) then
                  ()
                else (
                  ignore (
                    E.warn 
                      "Return value must reference memory in function %s\n" 
                      f.svar.vname);
                  ()
                )

            end

        | TFun (t, _, _, _) when (hasAttribute "sos_release" (typeAttrs t)) ->
            ignore (E.warn 
                      "Not allowed to set sos_release attribute.  Found in function %s\n"
                      f.svar.vname);
            ()
        | _ -> 
            ()
    end;
        
    DoChildren
                    
  method vstmt (s:stmt) =
    currentStmt := s;
    DoChildren

  method vinst (i:instr) =
 
    let alloced = U.getOwn i in
    let freed = U.get_released i in
    
      (* Match this instruction with any of: alloc, free, combo, or neither *)
      match ((List.length alloced > 0), (List.length freed > 0)) with
          
          (* Neither -> move on to next instruction *)  
          (false, false) -> 
            DoChildren

        (* Alloc -> insure that data is 'taken' exactly once on each path
         * between here and a return, or between here and a new overriding
         * alloc site.  Debug output listing alloc site and validity (with
         * proof). *)  
        | (true, false) ->
            begin
              match (allocDataTreatment alloced !currentStmt (Some i)) with
                  Store ->
                    DoChildren
                | _ -> 
                    ignore (E.warn 
                              "Alloced data from instruction %a\nis not stored\n" 
                              d_instr i);
                    DoChildren
            end


        (* Free -> insure that data is treated as 'dead' on each path between
         * here and a return, or between here and a new overriding alloc site.
         * Debug output listing free site and validity (with proof). *)  
        (* TODO: Looping specfic warning output is missing due to the limits of
         * the is_dead interface *)
        | (false, true) ->
            
            List.iter 
              (fun e ->

                 if !dbg_free_exp then
                   ignore (printf 
                             "FREE EXP: Statement %a frees: %a\n" 
                             d_stmt s d_exp e);
                 
                 if not (is_dead e s) then
                   ignore (E.warn 
                             "Potential access to dead data freed in instruction %a\n"
                             d_instr i);
              )
              freed;

            DoChildren

        (* Combo -> flag an error since CIL should prohibit this *)  
        | (true, true) ->
            ignore (E.bug "Instruction may not both free and alloc");
            DoChildren


end

class ptrArithVisitor = object
  inherit nopCilVisitor

  method vexpr (e : exp) : exp visitAction =
    let find_pointer_arithmetic e = 
      match e with
          BinOp (_, e1, _, _) when (isPointerType (typeOf e1) || isArrayType (typeOf e1)) -> 
            if !dbg_ptr_arith then
              ignore (printf "Dropping offset into pointer in expression %a\n" d_exp e);
            e1
        | _ -> 
            e
    
    in
      ChangeDoChildrenPost ((find_pointer_arithmetic e), (fun x -> x))
end


let openFile (what: string) (takeit: out_channel -> unit) (fl: string) = 
  if !E.verboseFlag then
    ignore (Printf.printf "Setting %s to %s\n" what fl);
  (try takeit (open_out fl)
   with _ ->
     raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))
;;

let outChannel : out_channel option ref = ref None;;

let argDescr = [

      (* Debugging for free *)
      
      ("--mem_dbg_free_exp", Arg.Unit (fun _ -> dbg_free_exp := true),
       "Print freed expressions");
     
      ("--mem_dbg_free_df", Arg.Unit (fun _ -> DF.dbg_free_df := true),
       "Dataflow specific debugging for the freed data flow");
     
      ("--mem_dbg_free_dead_s", Arg.Unit (fun _ -> DF.dbg_free_dead_s := true),
       "List if a non-instruction statemnt treats data as dead");
     
      ("--mem_dbg_free_dead_i", Arg.Unit (fun _ -> DF.dbg_free_dead_i := true),
       "List if an instruction treats data as dead");
     
      ("--mem_dbg_free_combine", Arg.Unit (fun _ -> DF.dbg_free_combine := true),
       "Print all joins exectued in data flow examining freed data");
     
      (* Debugging for alloc *)

      ("--mem_dbg_alloc_exp", Arg.Unit (fun _ -> dbg_alloc_exp := true),
       "Print alloced expressions");
    
      ("--mem_dbg_alloc_stores", Arg.Unit (fun _ -> dbg_alloc_stores := true),
       "Print all stores located in code");
      
      ("--mem_dbg_alloc_df", Arg.Unit (fun _ -> OF.dbg_alloc_df := true),
       "Dataflow specific debuging for the alloc data flow");
      
      ("--mem_dbg_alloc_store_i", Arg.Unit (fun _ -> OF.dbg_alloc_store_i := true),
       "List if an instruction stores data in a store");
     
      ("--mem_dbg_alloc_store_s", Arg.Unit (fun _ -> OF.dbg_alloc_store_s := true),
       "List statement level debuging info abotu stores");
     
      ("--mem_dbg_alloc_combine", Arg.Unit (fun _ -> OF.dbg_alloc_combine := true),
       "Print all joins exectued in data flow examining alloced data");
     
      (* Debugging for alias analysis *)

      ("--mem_dbg_may_alias", Arg.Unit (fun _ -> U.dbg_may_alias := true),
       "List all may alias queries and the results");
     
      ("--mem_dbg_equiv_i", Arg.Unit (fun _ -> IE.dbg_equiv_i := true),
       "Show equiv dataflow processing each instruction");

      ("--mem_dbg_equiv_combine", Arg.Unit (fun _ -> IE.dbg_equiv_combine := true),
       "Show equiv dataflow joins");

      ("--mem_dbg_equiv_stmt_summary", Arg.Unit (fun _ -> IE.dbg_equiv_stmt_summary := true),
       "Dump summary of equivalence sets during dataflow");

      ("--mem_dbg_equiv_df", Arg.Unit (fun _ -> IE.dbg_equiv_df := true),
       "Internal dataflow debug");

      (* Degbugging for the fill analysis *)
      
      ("--mem_dbg_fill_combine", Arg.Unit (fun _ -> FF.dbg_fill_combine := true),
       "List joins in fill dataflow");
     
      ("--mem_dbg_fill_i", Arg.Unit (fun _ -> FF.dbg_fill_i := true),
       "Show handling of instructions in the fill flow");
     
      ("--mem_dbg_fill_s", Arg.Unit (fun _ -> FF.dbg_fill_s := true),
       "Show handling of statements in the fill flow");
     
      (* Degbugging for the return analysis *)
      
      ("--mem_dbg_return_combine", Arg.Unit (fun _ -> RF.dbg_return_combine := true),
       "List joins in return dataflow");
     
      ("--mem_dbg_return_i", Arg.Unit (fun _ -> RF.dbg_return_i := true),
       "Show handling of instructions in the return flow");
     
      ("--mem_dbg_return_s", Arg.Unit (fun _ -> RF.dbg_return_s := true),
       "Show handling of statements in the return flow");
     
      (* Other random options *)
      
      ("--mem_dbg_ptr_arith", Arg.Unit (fun _ -> dbg_ptr_arith := true),
       "Note when pointer arithmatic is DE_STROY_ED!!!  Ka boom.");
     
      ("--enable_loop", Arg.Unit (fun _ -> DF.enable_loop := true),
       "Enable listing of data freed in loops.");
    
      ("--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
                "do not remove the unused variables and types");

      ("--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
             "the name of the output CIL file");
    
    ];;
  
    
let doFile fn = 
   
  let f = Frontc.parse fn () in  

    (* Execute other modules in the correct order *) 
    ignore (Simplemem.feature.fd_doit f);
    ignore (Simplify.feature.fd_doit f);
    ignore (Oneret.feature.fd_doit f);
    ignore (MakeOneCFG.feature.fd_doit f);
    ignore (Ptranal.feature.fd_doit f);
    ignore (AddAnnotations.feature.fd_doit f);

    (* Generate a table to note persistent stores *)
    (* TODO: this assumes that the store is a global *)
    OF.stores := 
    foldGlobals 
      f
      (fun s g -> match g with
           GVarDecl (v, l)
         | GVar (v, _, l) ->
             if (!dbg_alloc_stores) then 
               ignore (printf "ALLOC STORES: Store var: %s\n" v.vname);
             (Lval (var v))::s
       | GFun (fd, l) -> s
       | _ -> s
    ) 
    [];

    let mVisitor = new memoryVisitor in
      visitCilFileSameGlobals mVisitor f;

      (match !outChannel with
           None -> ()
         | Some c -> Stats.time "printCIL" 
                       (dumpFile (!printerForMaincil) c f.fileName) f);
;;

let mainFunction () =

  let usageMsg = "Usage: memory [options] source-file" in
  
  let fileNames : string list ref = ref [] in
  
  let recordFile fname = 
    fileNames := fname :: (!fileNames) 
  in
  
  let outName = ref "" in
    
    Arg.parse argDescr recordFile usageMsg;
    
    Cil.initCIL ();

    fileNames := List.rev !fileNames;
    
   
    if not (List.length !fileNames == 1) then (
      Arg.usage argDescr usageMsg;
      exit 0;
    );
      
    List.iter doFile !fileNames;
;;
    
    

mainFunction ();;
      


