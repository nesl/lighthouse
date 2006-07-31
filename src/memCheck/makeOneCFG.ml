(*
 * File: makeOneCFG
 * 
 * Date: 2/06
 * 
 * Purpose: Force a program to have (about) one instruction in each CIL
 * statement.
 * 
 * Author: Roy Shea
 * 
 * Heritage: 
 *
 * - Class makeOneCFG is identical (sans name) to to callBBVisitor from the
 * Partial module included in the 1.3.5 CIL release.
 *
 * - Class vidVisitor is identical to the implementation in Partial module 
 * included in the 1.3.5 CIL release.
 *
 * These two classes are reproduced here to provide a small static code base
 * to create a simpler CFG for instruction level dataflow.
 *
 * See below for origonal copy right notice.
 *)


open Cil
open Pretty


(* Transform instruction lists into a block of statments each containing one
 * instruction. *)
class makeOneCFG = object
  inherit nopCilVisitor 

  method vstmt s =
    match s.skind with
        Instr(il) -> begin
          
          let list_of_stmts = 
            List.map (fun one_inst -> mkStmtOneInstr one_inst) il 
          in
          
          let block = mkBlock list_of_stmts 
          in
            
            ChangeDoChildrenPost 
              (s, (fun _ -> s.skind <- Block(block); s))

        end
      | _ -> DoChildren
end 


(* A transformation that gives each variable a unique identifier. *)
class vidVisitor = object
  inherit nopCilVisitor 
  val count = ref 0 

  method vvdec vi = 
    vi.vid <- !count ;
    incr count ; SkipChildren
end 


(* Feature description *)
let feature : featureDescr = {
  fd_name = "makeOneCFG";
  fd_enabled = ref true;
  fd_description = "make the program look more like a CFG with one instruction per statement";
  fd_extraopt = [];
  fd_doit = 
    (fun f -> 
       let oVisitor = new makeOneCFG in
       let vVisitor = new vidVisitor in
         
         visitCilFileSameGlobals oVisitor f;  
         visitCilFileSameGlobals vVisitor f;
         iterGlobals 
           f 
           (fun glob -> 
              match glob with
                  GFun(fd,_) -> prepareCFG fd; ignore (computeCFGInfo fd true)
                | _ -> ()
           ) 
    );
  fd_post_check = true;
} 
;;



(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

