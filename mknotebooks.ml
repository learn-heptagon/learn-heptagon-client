open Notebook

let read_file filename =
  let ic = open_in filename in
  let rec aux acc =
    try
      aux ((input_line ic)::acc)
    with End_of_file -> List.rev acc
  in
  let lines = aux [] in
  close_in ic;
  String.concat "\n" lines

let notebooks : notebook list ref = ref []

let parse_index s =
  let json = Yojson.Safe.from_string s in
  match json with
  | `Assoc l ->
     let title =
       match List.assoc_opt "title" l with
       | Some (`String s) -> s
       | _ -> invalid_arg "parse_index"
     and content =
       match List.assoc_opt "content" l with
       | Some (`List l) ->
          List.map (function `String s -> s | _ -> invalid_arg "parse_index") l
       | _ -> invalid_arg "parse_index"
     and has_autocorrect =
       match List.assoc_opt "has-autocorrect" l with
       | Some (`Bool b) -> b
       | _ -> false
     in (title, has_autocorrect, content)
  | _ -> invalid_arg "parse_index"

let new_id =
  let id = ref 0 in
  fun () -> id := !id + 1; !id

let read_cell_file dirname filename =
  let file = Filename.concat dirname filename in
  match Filename.extension filename with
  | ".html" -> Html (read_file file)
  | ".lus" -> Editor {
                  editor_id = new_id ();
                  editor_title = Filename.remove_extension filename;
                  editor_content = read_file file
                }
  | ext -> invalid_arg (Printf.sprintf "read_cell_file (%s)" ext)

let add_notebook dirname =
  let index = read_file (Filename.concat dirname "index.json") in
  (* print_endline index; *)
  let (title, has_autocorrect, cells) = parse_index index in
  let cells = List.map (read_cell_file dirname) cells in
  let notebook = { title; has_autocorrect; cells } in
  notebooks := notebook::!notebooks

let print_cell ff = function
  | Heading s -> Format.fprintf ff "Heading %S;@." s
  | Text s -> Format.fprintf ff "Text %S;@." s
  | Html s -> Format.fprintf ff "Html %S;@." s
  | Editor { editor_id; editor_title; editor_content } ->
     Format.fprintf ff
       "Editor { editor_id = %d; editor_title = %S; editor_content = %S };@."
       editor_id editor_title editor_content

let print_notebook ff { title; has_autocorrect; cells } =
  Format.fprintf ff "{ @[<v 2>";
  Format.fprintf ff "title = %S;@." title;
  Format.fprintf ff "has_autocorrect = %b;@." has_autocorrect;
  Format.fprintf ff "cells = [";
  List.iter (print_cell ff) cells;
  Format.fprintf ff "] }@];@."

let print_notebooks ff notebooks =
  Format.fprintf ff "open Notebook@.@.";
  Format.fprintf ff "let notebooks = [@.";
  List.iter (print_notebook ff) notebooks;
  Format.fprintf ff "]@."

let help = "Usage: ocaml mknotebooks.ml <json files>"

let () =
  Arg.parse [] add_notebook help;
  let oc = open_out "notebooks.ml" in
  let fmt = Format.formatter_of_out_channel oc in
  print_notebooks fmt (List.rev !notebooks);
  close_out oc
