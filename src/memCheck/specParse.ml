(*pp camlp4o *)
(* The above comment is required to inform the build system of the preprocessor
 * to be used. *)

open Cil;;
open Genlex;;
open Pretty;;

module E = Errormsg;;

exception Parse_error;;

type store_state = {
  full:string list;
  empty:string list;
  heap:string list;
};;


type spec_block = 
    Stores of string list
  | Pre of (string * store_state)
  | Post of (string * store_state)
;;


let keywords = [
  "{";
  "}";
  "(";
  ")";
  ";";
  ".";

  "stores";

  "pre";
  "post";

  "empty";
  "full";
  "heap";
];;


let spec_lexer (c: in_channel) = 
  Genlex.make_lexer keywords (Stream.of_channel c)
;;


let parse_spec_block s =

  let parse_stores s =
    let rec parse_stores_helper s stores =
      match s with parser 
        | [<'Ident s_name; 'Kwd ";">] ->
            parse_stores_helper s (s_name :: stores)
        | [<'Kwd "}"; 'Kwd ";">] ->
            Stores stores
        | [<>] -> raise Parse_error
    in
      parse_stores_helper s []
  in

  let rec parse_pre_post s f e h =
    match s with parser
      | [<'Ident s_name; 'Kwd ".">] -> begin
          match s with parser 
            | [<'Kwd "empty"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s f (s_name::e) h
            | [<'Kwd "full"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s (s_name::f) e h
            | [<'Kwd "heap"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s f e h
            | [<>] -> raise Parse_error
        end
      | [<'Kwd "}">] ->
          {full=f; empty=e; heap=h}
      | [<>] -> raise Parse_error
  in

  let parse_pre f_name s = 
    Pre (f_name, parse_pre_post s [] [] [])
  in

  let parse_post f_name s = 
    Post (f_name, parse_pre_post s [] [] [])
  in

    match s with parser
      | [<'Kwd "stores"; 'Kwd "{">] ->
          parse_stores s
      | [<'Ident f_name; 'Kwd ".">] -> begin
          match s with parser 
            | [<'Kwd "pre"; 'Kwd "{">] ->
                parse_pre f_name s
            | [<'Kwd "post"; 'Kwd "{">] ->
                parse_post f_name s
            | [<>] -> raise Parse_error
        end
      | [<>] -> raise Parse_error
;;


let pretty_print_block b = 
  match b with 
      Stores stores ->
        ignore ("Found store block with stores:\n");
        List.iter (fun s -> ignore (printf "  %s\n" s)) stores;
        ()
    | Pre (f_name, state) ->
        ignore (printf "Found pre-condition block for %s:\n" f_name);
        ignore (printf "  Full stores:\n");
        List.iter (fun s -> ignore (printf "    %s\n" s)) state.full;
        ignore (printf "  Empty stores:\n");
        List.iter (fun s -> ignore (printf "    %s\n" s)) state.empty;
        ()
    | Post (f_name, state) ->
        ignore (printf "Found post-condition block for %s:\n" f_name);
        ignore (printf "  Full stores:\n");
        List.iter (fun s -> ignore (printf "    %s\n" s)) state.full;
        ignore (printf "  Empty stores:\n");
        List.iter (fun s -> ignore (printf "    %s\n" s)) state.empty;
        ()
;;


let lookup_post blocks name =
  let block = 
    try
      List.find 
        (fun b -> 
           match b with 
             | Post (f_name, state) when (f_name = name) -> true
             | _ -> false
        ) 
        blocks
    with 
        Not_found -> 
          E.warn "Failed to find post state for function: %s" name;
          Post (name, {full=[]; empty=[]; heap=[]})
  in
    match block with
        Post (_, state) -> (state.full, state.empty, state.heap)
      | _ -> E.s (E.error "Huh?")
;;


let lookup_pre blocks name =
  let block = 
    try
      List.find 
        (fun b -> 
           match b with 
             | Pre (f_name, state) when (f_name = name) -> true
             | _ -> false
        ) 
        blocks
    with 
        Not_found -> 
          E.warn "Failed to find pre state for function: %s" name;
          Pre (name, {full=[]; empty=[]; heap=[]})
  in

    match block with
        Pre (_, state) -> (state.full, state.empty, state.heap)
      | _ -> E.s (E.error "Huh?")
;;


let parse_spec file_name =

  let blocks = ref [] in

  let block_num = ref 1 in

  let spec_stream = spec_lexer (open_in file_name) in
 
    try 
      while true do
        let next = parse_spec_block spec_stream in
          blocks := (next :: !blocks);
          block_num := !block_num + 1;
      done;
      assert false
    with
      | Parse_error ->
            Stream.empty spec_stream;
            blocks := List.rev !blocks;
            !blocks

      | Stream.Error s -> 
          E.error "*** Error in block %d ***\n" !block_num;
          raise (Stream.Error s)
                         
;;


(*
let main _ = 

  if Array.length Sys.argv != 2 then (
    prerr_endline (Printf.sprintf "usage: %s <spec file>" Sys.argv.(0));
    exit 1
  );

  let spec = ignore (parse_spec Sys.argv.(1)) in

    List.iter pretty_print_block spec
;;
 *)

