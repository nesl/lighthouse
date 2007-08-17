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
  "unknown";

];;



type spec_block = 
    Pre of (string * i2c_state)
  | Post of (string * i2c_state)
;;



let parse_spec_block s =

  let rec parse_pre_post s =
    match s with parser
      | [<'Kwd "$">] -> begin
           match s with parser
             | [<'Kwd "reserved"; 'Kwd "("; 'Kwd ")"; 'Kwd ";"; 'Kwd "}">] ->
                 Reserved
             | [<'Kwd "free"; 'Kwd "("; 'Kwd ")"; 'Kwd ";"; 'Kwd "}">] ->
                 Free
             | [<'Kwd "unknown"; 'Kwd "("; 'Kwd ")"; 'Kwd ";"; 'Kwd "}">] ->
                 Unknown
             | [<>] -> 
                 raise Parse_error
         end
      | [<>] -> 
          raise Parse_error
  in


  let parse_pre f_name s = 
    Pre (f_name, parse_pre_post s)
  in


  let parse_post f_name s = 
    Post (f_name, parse_pre_post s)
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



let pretty_print_block (b: spec_block) = 
  match b with 
    | Pre (f_name, state) ->
        let state_str = i2c_state_to_string state in
          ignore (printf "Found pre-condition block for %s:\n" f_name);
          ignore (printf "  %s\n" state_str);
          ()
    | Post (f_name, state) ->
        let state_str = i2c_state_to_string state in
          ignore (printf "Found post-condition block for %s:\n" f_name);
          ignore (printf "  %s\n" state_str);
          ()
;;



let parse_spec_stream spec_stream =

  let block_num = ref 0 in

  let specification = {pre=[]; post=[]} in
 
    try 
      while true do
        block_num := !block_num + 1;
        match parse_spec_block spec_stream with
            Pre pre -> specification.pre <- pre::(specification.pre)
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


let parse_spec_file file_name = 
  let file_in = open_in file_name in
  let spec_stream = Genlex.make_lexer keywords (Stream.of_channel file_in) in
   parse_spec_stream spec_stream 
;;



let parse_spec_string str = 
  let spec_stream = Genlex.make_lexer keywords (Stream.of_string str) in
   parse_spec_stream spec_stream 
;;



