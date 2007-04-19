#load "odot.cma";;

open Odot;;
module H = Hashtbl;;

type dot_table = (id, id list) H.t;;

let mapping: dot_table = H.create 17

(* Helper function to add elements to a hash table *)
let update_table a b table =
  if H.mem table a then
    H.replace table a (b @ (H.find table a))
  else
    H.add table a b


(* Print basic usage instructions *)
let usage () =
  prerr_endline (Printf.sprintf "usage: %s <file>" Sys.argv.(0));
  exit 1


(* Print all nodes seen in the file *)
let generate_table (g:graph) table =

  let nodes_from_stmt s =
    match s with
        Stmt_node ((id1, _), _) -> 
          update_table id1 [] table;
          ()
      | Stmt_edge (Edge_node_id (id1, _), [Edge_node_id (id2, _)], _) -> 
          update_table id1 [id2] table;
          update_table id2 [] table;
          ()
      | _ -> prerr_endline (
          Printf.sprintf 
            "unable to handle: %s <file>" 
            (string_of_stmt g.kind s)
        )
  in
    List.iter nodes_from_stmt g.stmt_list
  

    
(* Print the states in table *) 
let print_states table =

  H.iter 
    (fun a b -> 
       Printf.printf "%s:\n" (string_of_id a);
       List.iter (fun n -> Printf.printf " -> %s\n" (string_of_id n)) b;
       Printf.printf "\n"
    ) 
    table

    
(* Given the state mapping generate an enum *)
let print_enum table = 

  Printf.printf "enum State {\n";
  H.iter (fun a b -> Printf.printf "    dfs_%s,\n" (string_of_id a)) table;
  Printf.printf "};\n";
  ()
    
    
(* Print a cascading "else if" statement for a given state and its transitions
 * *)
let print_transition_check table =
  
  let print_transition from_state to_list =  
  
    (* Ignore impty lists *)
    if List.length to_list < 1 then
      ();
  
    let print_to_state to_state = 
      Printf.printf "if (new == dfs_%s) {\n            return true;\n" (string_of_id to_state);
      Printf.printf "        } else ";
      ()
    in

      Printf.printf "    if (old == dfs_%s) {\n        " (string_of_id from_state);
      List.iter print_to_state to_list;
      Printf.printf "{\n            return false;\n        }\n    }\n\n";
      ()

  in

    Printf.printf "bool valid_transition(enum State old, enum State new) {\n\n";
    H.iter print_transition table;
    Printf.printf "    return false;\n";
    Printf.printf "}\n";
    ()

      


(* Driver function *)    
let main () =

  if Array.length Sys.argv < 2 then
    usage ();

  let g = Odot.parse_file Sys.argv.(1) in
    
    generate_table g mapping;

    (* print_states mapping; *)
      
    Printf.printf "\n";
    print_enum mapping;
    Printf.printf "\n";
    print_transition_check mapping;
    
    ()
  

(* Do something!!! *)
let _ = main ()

