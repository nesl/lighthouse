(*pp camlp4o *)
(* The above comment is required to inform the build system of the preprocessor
 * to be used. *)

open State;;
open Genlex;;
open Pretty;;

module E = Errormsg;;

exception Parse_error;;

let keywords = [
  "{";
  "}";
  "(";
  ")";
  ";";
  ".";

  "$";

  "pre";
  "post";

  "reserved";
  "free";

];;

type spec_block = 
    Reserved of string
  | Free of string
;;


let parse_spec_block s =

  let rec parse_pre_post s f e h =
    match s with parser
      | [<'Kwd "$">] -> begin
           match s with parser
             | [<'Ident s_name; 'Kwd ".">] -> begin
                 match s with parser 
                   | [<'Kwd "mpty"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                       parse_pre_post s f ((Char.escaped '$' ^ s_name)::e) h
                   | [<'Kwd "full"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                       parse_pre_post s ((Char.escaped '$' ^ s_name)::f) e h
                   | [<'Kwd "heap"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                       parse_pre_post s f e h
                   | [<>] -> raise Parse_error
               end
             | [<'Float count>] -> begin
                 match s with parser 
                   | [<'Kwd "mpty"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                       parse_pre_post s f ((Char.escaped '$' ^ (string_of_float count))::e) h
                   | [<'Kwd "full"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                       parse_pre_post s ((Char.escaped '$' ^ (string_of_float count))::f) e h
                   | [<'Kwd "heap"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                       parse_pre_post s f e h
                   | [<>] -> raise Parse_error
               end
         end
      | [<'Ident s_name; 'Kwd ".">] -> begin
          match s with parser 
            | [<'Kwd "mpty"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s f (s_name::e) h
            | [<'Kwd "full"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s (s_name::f) e h
            | [<'Kwd "heap"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s f e h
            | [<>] -> raise Parse_error
        end
      | [<'Kwd "}">] ->
          {full=f; empty=e; heap=h}
      | [<>] -> 
          raise Parse_error
  in

  let rec parse_memory s (mem_funcs: (string * int) list): (string * int) list =
    match s with parser 
      | [<'Ident f_name; 'Int index>] ->
          parse_memory s ((f_name, index)::mem_funcs)
      | [<'Kwd "}">] ->
          mem_funcs
      | [<>] -> 
          raise Parse_error
  in

  let parse_pre f_name s = 
    Pre (f_name, parse_pre_post s [] [] [])
  in

  let parse_post f_name s = 
    Post (f_name, parse_pre_post s [] [] [])
  in

    match s with parser
      | [<'Ident f_name; 'Kwd ".">] -> begin
          match s with parser 
            | [<'Kwd "pre"; 'Kwd "{">] ->
                parse_pre f_name s
            | [<'Kwd "post"; 'Kwd "{">] ->
                parse_post f_name s
            | [<>] -> raise Parse_error
        end
      | [<>] -> 
          raise Parse_error
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
    | Alloc mem_funcs ->
        ignore (printf "Found allocation functions:\n");
        List.iter 
          (fun (f_name, id) -> 
             ignore (printf "    Function %s allocates formal %d\n" f_name id)
        )
          mem_funcs
    | Dealloc mem_funcs ->
        ignore (printf "Found deallocation functions:\n");
        List.iter 
          (fun (f_name, id) -> 
             ignore (printf "    Function %s frees formal %d\n" f_name id)
        )
          mem_funcs

;;


let parse_spec_stream spec_stream =

  let block_num = ref 0 in

  let specification = {stores=[]; pre=[]; post=[]} in
  let alloc_funcs = ref [] in
  let free_funcs = ref [] in
 
    try 
      while true do
        block_num := !block_num + 1;
        match parse_spec_block spec_stream with
            Stores stores -> specification.stores <- stores@(specification.stores)
          | Pre pre -> specification.pre <- pre::(specification.pre)
          | Post post -> specification.post <- post::(specification.post)
          | Alloc funcs -> alloc_funcs :=  funcs@(!alloc_funcs)
          | Dealloc funcs -> free_funcs :=  funcs@(!free_funcs)
      done;
      assert false
    with
      | Parse_error ->
            Stream.empty spec_stream;
            (specification, (!alloc_funcs, !free_funcs))

      | Stream.Error s -> 
          E.error "*** Error in block %d ***\n" !block_num;
          raise (Stream.Error s)
                         
;;


let parse_spec_file file_name = 
  let file_in = open_in file_name in
  let spec_stream = Genlex.make_lexer keywords (Stream.of_channel file_in) in
   parse_spec_stream spec_stream 
;;



let parse_spec_string str = 
  let spec_stream = Genlex.make_lexer keywords (Stream.of_string str) in
   parse_spec_stream spec_stream 
;;



