#load "str.cma"

type check_point_line = 
    SKIP 
  | LINE of int 
  | EDGE of string * string
  | ERROR of string
;;


exception Dot_parse of string;;
exception Out_of_order;;
exception Abort;;


                       
(* Print basic usage instructions *)
let usage () =
  prerr_endline (Printf.sprintf "usage: %s <c file> <dot file>" Sys.argv.(0));
  exit 1


(* Parse file one line at a time *)
let parse_file input =

  (** Primary parsing function **)
  let simple_parse line = 
  
    (* Yum.  Regexp taste good... *)
    let line_skip_re = Str.regexp "digraph *[a-zA-Z_]* *{\\|}\\|\\(^ *$\\)" in
    let line_num_re = Str.regexp "^ *// *\\([0-9]*\\) *$" in
    let line_edge_re = Str.regexp "^ *\\([a-zA-Z0-9_]*\\) *-> *\\([a-zA-Z0-9_]*\\); *$" in

    (* Line number *)
    let match_line_num line =
      try
        ignore (Str.search_forward line_num_re line 0);
        Some (Str.matched_group 1 line)
      with
          Not_found -> None
    in

    (* Dot edge *)
    let match_line_edge line = 
      try
        ignore(Str.search_forward line_edge_re line 0);
        Some (Str.matched_group 1 line, Str.matched_group 2 line)
      with
          Not_found -> None
    in

    (* Other line type to skip *)
    let match_line_skip line =
      try 
        ignore(Str.search_forward line_skip_re line 0);
        Some line
      with
          Not_found -> None
    in


      match ((match_line_skip line), (match_line_num line), (match_line_edge line)) with 
          (Some _, None, None) -> SKIP
        | (None, Some num, None) -> LINE (int_of_string num)
        | (None, None, Some (s1, s2)) -> EDGE (s1, s2)
        | (_, _, _) -> ERROR line
  in


  let rec get_next_edge input =
    try
      let s = input_line input in
        match simple_parse s with
            SKIP -> get_next_edge input
          | EDGE (n1, n2) -> (n1, n2) 
          | _ -> raise (Dot_parse s)
        with
            End_of_file -> 
              Printf.printf "Unexpected end of file\n";
              raise Abort
          | Dot_parse line -> 
              Printf.printf "Invalid line: %s\n" line;
              raise Abort
  in
    
  let rec get_next_num input =
    try
      let s = input_line input in
        match simple_parse s with
            SKIP -> get_next_num input
          | LINE num -> num
          | _ -> raise (Dot_parse s)
    with
        End_of_file ->
          (-1)
      | Dot_parse line ->
          Printf.printf "Invalid line: %s\n" line;
          raise Abort
  in

  let rec build_call_list input call_list =
    try
      let next_num = get_next_num input in

        if next_num == -1 then
          call_list
        else (
          let next_edge = get_next_edge input in
            build_call_list input ((next_num, next_edge):: call_list)
        )
    with
        _ -> Printf.printf "Bummer\n"; []
  in
              
  (** Test function **) 
    (*
    let call_list = build_call_list input [] in
      List.iter 
        (fun (line, (e1, e2)) -> 
           Printf.printf "One line %d edge from %s to %s\n" line e1 e2
        ) 
        call_list
     *)
    
    build_call_list input []

    
let make_call_check edge = 
  let (e1, e2) = edge in

  (* Generate the check *)
  let check = Printf.sprintf "if (valid_transition(s->dfs_state, dfs_%s) != true)\n" e2 in
  let fail = Printf.sprintf "{\n    exit(-1);\n}" in

    check ^ fail
    
  
let embed_checks input parse_list = 
   
  let line_num = ref 0 in

  let rec print_through_check input check =
    try
      let (num, edge) = check in
        if !line_num == num then (
          Printf.printf "%s\n" (make_call_check edge)
        ) else if !line_num > num then (
          raise Out_of_order
        ) else (
          Printf.printf "%s\n" (input_line input);
          line_num := !line_num + 1;
          print_through_check input check
        )
    with
        End_of_file ->
          Printf.printf "Unexpected end of file\n"
      | Out_of_order ->
          Printf.printf "The parse_list is out of order.  Sort and then try again.\n"
  in

    List.iter (print_through_check input) parse_list;

    try
      while true do (
        let s = input_line input in
          Printf.printf "%s\n" s
      )
      done
    with
        End_of_file ->
          Printf.printf "\n"

    

(* Helper function to get rid of repeated checks *)
let rec squish_list parse_list =
  match parse_list with
      [] -> []
    | hd::[] -> [hd]
    | (num1, (e11, e12))::(num2, (e21, e22))::tl when (num1 = num2) && (e12 = e22) ->
        (num1, (e11, e12)) :: squish_list tl
    | hd::tl -> hd :: squish_list tl 
    

(* Driver function *)
let main () =
  
  if Array.length Sys.argv < 3 then
    usage ();
    
  let c_file = open_in Sys.argv.(1) in
  let dot_file = open_in Sys.argv.(2) in
    
  let parse_list = parse_file dot_file in
  let parse_list = List.sort (fun (num1, _) (num2,_) -> num1 - num2) parse_list in
  let parse_list = squish_list parse_list in

    embed_checks c_file parse_list
  

(* Do something!!! *)
let _ = main ()


