open Genlex;;

exception Parse_error;;

type store_state = {
  full:string list;
  empty:string list;
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

  let rec parse_pre_post s f e =
    match s with parser
      | [<'Ident s_name; 'Kwd ".">] -> begin
          match s with parser 
            | [<'Kwd "empty"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s f (s_name::e)
            | [<'Kwd "full"; 'Kwd "("; 'Kwd ")"; 'Kwd ";">] ->
                parse_pre_post s (s_name::f) e
            | [<>] -> raise Parse_error
        end
      | [<'Kwd "}">] ->
          {full=f; empty=e}
      | [<>] -> raise Parse_error
  in

  let parse_pre f_name s = 
    Pre (f_name, parse_pre_post s [] [])
  in

  let parse_post f_name s = 
    Post (f_name, parse_pre_post s [] [])
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
        Printf.printf ("Found store block with stores:\n");
        List.iter (fun s -> Printf.printf "  %s\n" s) stores;
        ()
    | Pre (f_name, state) ->
        Printf.printf "Found pre-condition block for %s:\n" f_name;
        Printf.printf "  Full stores:\n";
        List.iter (fun s -> Printf.printf "    %s\n" s) state.full;
        Printf.printf "  Empty stores:\n";
        List.iter (fun s -> Printf.printf "    %s\n" s) state.empty;
        ()
    | Post (f_name, state) ->
        Printf.printf "Found post-condition block for %s:\n" f_name;
        Printf.printf "  Full stores:\n";
        List.iter (fun s -> Printf.printf "    %s\n" s) state.full;
        Printf.printf "  Empty stores:\n";
        List.iter (fun s -> Printf.printf "    %s\n" s) state.empty;
        ()
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
            List.iter pretty_print_block !blocks;
            !blocks

      | Stream.Error s -> 
          Printf.printf "*** Error in block %d ***\n" !block_num;
          raise (Stream.Error s)
                         
;;


let usage () =
  prerr_endline (Printf.sprintf "usage: %s <spec file>" Sys.argv.(0));
  exit 1
;;


let main _ = 

  if Array.length Sys.argv != 2 then
    usage ();
        
  ignore (parse_spec Sys.argv.(1))
;;

main ()

