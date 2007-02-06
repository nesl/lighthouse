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

  "stores";

  "pre";
  "post";

  "mpty";
  "full";
  "heap";
];;

type spec_block = 
    Stores of string list
  | Pre of (string * spec_store)
  | Post of (string * spec_store)
;;


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



let parse_spec file_name =

  let block_num = ref 0 in

  let specification = {stores=[]; pre=[]; post=[]} in

  let spec_stream = spec_lexer (open_in file_name) in
 
    try 
      while true do
        block_num := !block_num + 1;
        match parse_spec_block spec_stream with
            Stores stores -> specification.stores <- stores@(specification.stores)
          | Pre pre -> specification.pre <- pre::(specification.pre)
          | Post post -> specification.post <- post::(specification.post)
      done;
      assert false
    with
      | Parse_error ->
            Stream.empty spec_stream;
            specification

      | Stream.Error s -> 
          E.error "*** Error in block %d ***\n" !block_num;
          raise (Stream.Error s)
                         
;;

