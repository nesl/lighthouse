#load "odot.cma";;

open Odot;;
module H = Hashtbl;;

type dot_table = (id, id list) H.t;;

let mapping: dot_table = H.create 17;;

(* Helper function to add elements to a hash table *)
let update_table a b table =
  if H.mem table a then
    H.replace table a (b @ (H.find table a))
  else
    H.add table a b
;;


(* Print basic usage instructions *)
let usage () =
  prerr_endline (Printf.sprintf "usage: %s <file>" Sys.argv.(0));
  exit 1
;;


type transition = {
  from_func:string; 
  to_func:string; 
  pre:string;
  post:string;
}
;;


let get_transition (attrs: Odot.attr list): (string * string) = 

  let pre_string = match Odot.attr_value (Simple_id "taillabel") attrs with
      Some (Double_quoted_id str) -> str
    | Some _
    | None -> 
        ignore (
          Printf.printf "Unable to find taillabel in attribute list: %s\n" 
            (string_of_attr_list attrs)
        );
        ""
  in
  
  let post_string = match Odot.attr_value (Simple_id "headlabel") attrs with
      Some (Double_quoted_id str) -> str
    | Some _
    | None -> 
        ignore (
          Printf.printf "Unable to find headlabel in attribute list: %s\n" 
            (string_of_attr_list attrs)
        );
        ""
  in

    (pre_string, post_string)

;;


(* Print all nodes seen in the file *)
let generate_table (g:graph) table constraints =

  let nodes_from_stmt s =
    match s with
        Stmt_node ((id1, _), _) -> 
          update_table id1 [] table;
          ()
      | Stmt_edge (Edge_node_id (id1, _), [Edge_node_id (id2, _)], attrs) -> 
          update_table id1 [id2] table;
          update_table id2 [] table;
          let (pre, post) = get_transition attrs in
            constraints := {from_func=(string_of_id id1); 
                            to_func=(string_of_id id2); 
                            pre=pre; 
                            post=post} :: !constraints;
            ()
      | _ -> prerr_endline (
          Printf.sprintf 
            "unable to handle statement: %s" 
            (string_of_stmt g.kind s)
        )
  in

    List.iter nodes_from_stmt g.stmt_list
;;
  

    
(* Print the states in table *) 
let print_states table =

  H.iter 
    (fun a b -> 
       Printf.printf "%s:\n" (string_of_id a);
       List.iter (fun n -> Printf.printf " -> %s\n" (string_of_id n)) b;
       Printf.printf "\n"
    ) 
    table
;;

    
(* Given the state mapping generate an enum *)
let print_enum table = 

  Printf.printf "enum State {\n";
  H.iter (fun a b -> Printf.printf "    dfs_%s,\n" (string_of_id a)) table;
  Printf.printf "};\n";
  ()
;;
    
    
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
;;


let print_constraints constraints =

  let print_constraint edge =

    Printf.printf "Calling %s after finishing function %s\n" edge.to_func edge.from_func;

    Printf.printf "%s.pre() {\n" edge.to_func;
    Printf.printf "%s\n" edge.pre;
    Printf.printf "}\n\n";

    Printf.printf "%s.post() {\n" edge.to_func;
    Printf.printf "%s" edge.post;
    Printf.printf "}\n\n";

  in

    List.iter print_constraint constraints
;;

      
(* Driver function *)    
let main () =

  if Array.length Sys.argv < 2 then
    usage ();

  let constraints = ref [] in

  let g = Odot.parse_file Sys.argv.(1) in
    
    generate_table g mapping constraints;

    (* print_states mapping; *)
      
    Printf.printf "\n";
    print_enum mapping;
    Printf.printf "\n";
    print_transition_check mapping;
   
    Printf.printf "\n";
    print_constraints !constraints;

    ()
;;
  

(* Do something!!! *)
let _ = 
  main ()
;;
