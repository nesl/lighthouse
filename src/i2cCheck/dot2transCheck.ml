module H = Hashtbl;;

type dot_table = (Odot.id, Odot.id list) H.t;;

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

  let pre_string = match Odot.attr_value (Odot.Simple_id "taillabel") attrs with
      Some (Odot.Double_quoted_id str) -> str
    | Some _
    | None -> 
        ignore (
          Printf.printf "Unable to find taillabel in attribute list: %s\n" 
            (Odot.string_of_attr_list attrs)
        );
        ""
  in
  
  let post_string = match Odot.attr_value (Odot.Simple_id "headlabel") attrs with
      Some (Odot.Double_quoted_id str) -> str
    | Some _
    | None -> 
        ignore (
          Printf.printf "Unable to find headlabel in attribute list: %s\n" 
            (Odot.string_of_attr_list attrs)
        );
        ""
  in

    (pre_string, post_string)

;;


(* Print all nodes seen in the file *)
let generate_table (g:Odot.graph) table constraints =

  let nodes_from_stmt s =
    match s with
        Odot.Stmt_node ((id1, _), _) -> 
          update_table id1 [] table;
          ()
      | Odot.Stmt_edge 
          (Odot.Edge_node_id (id1, _), [Odot.Edge_node_id (id2, _)], attrs) -> 
          
          update_table id1 [id2] table;
          update_table id2 [] table;
          let (pre, post) = get_transition attrs in
            constraints := {from_func=(Odot.string_of_id id1); 
                            to_func=(Odot.string_of_id id2); 
                            pre=pre; 
                            post=post} :: !constraints;
            ()
      | _ -> prerr_endline (
          Printf.sprintf 
            "unable to handle statement: %s" 
            (Odot.string_of_stmt g.Odot.kind s)
        )
  in

    List.iter nodes_from_stmt g.Odot.stmt_list
;;
  

    
(* Print the states in table *) 
let print_states table =

  H.iter 
    (fun a b -> 
       Printf.printf "%s:\n" (Odot.string_of_id a);
       List.iter (fun n -> Printf.printf " -> %s\n" (Odot.string_of_id n)) b;
       Printf.printf "\n"
    ) 
    table
;;

    
(* Given the state mapping generate an enum *)
let print_enum table = 

  Printf.printf "enum State {\n";
  H.iter (fun a b -> Printf.printf "    dfs_%s,\n" (Odot.string_of_id a)) table;
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
      Printf.printf 
        "if (new == dfs_%s) {\n            return true;\n" 
        (Odot.string_of_id to_state);
      Printf.printf "        } else ";
      ()
    in

      Printf.printf 
        "    if (old == dfs_%s) {\n        " 
        (Odot.string_of_id from_state);
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


let table_to_string table =
 
  let handle_edge edge =

    let from_node = edge.from_func in
    let to_node = edge.to_func in
    let spec_string = "" in
    let spec_string = spec_string ^ Printf.sprintf "%s.pre {\n" edge.to_func in
    let spec_string = spec_string ^ Printf.sprintf "%s\n" edge.pre in
    let spec_string = spec_string ^ Printf.sprintf "}\n\n" in
    let spec_string = spec_string ^ Printf.sprintf "%s.post {\n" edge.to_func in
    let spec_string = spec_string ^ Printf.sprintf "%s\n" edge.post in
    let spec_string = spec_string ^ Printf.sprintf "}\n\n" in

      (from_node, to_node, spec_string)

  in

    List.map handle_edge table
;;


let print_constraints constraints =
  List.iter 
    (fun (from_node, to_node, spec) ->
       Printf.printf "Transition after %s to %s:\n%s"
         from_node
         to_node
         spec
    ) 
    (table_to_string constraints)
;;


let get_fsm_graph (file:string) =   

  let constraints = ref [] in
  let g = Odot.parse_file file in
  let _ = generate_table g mapping constraints in
  
  let pre_post_string = table_to_string !constraints in
    pre_post_string

;;


(* Driver function *)    
(*
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
*)

(* Do something!!! *)
(*
let _ = 
  main ()
;;
*)
