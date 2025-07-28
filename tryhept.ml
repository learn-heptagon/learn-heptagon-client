open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

let editors_of_contents content_list nob_id =
  (List.mapi (fun i c ->
    Page.Editor {
      editor_id = nob_id * 10 + i + 1;
      editor_title = "editor" ^ string_of_int (nob_id * 10 + i + 1);
      editor_content = c;
    }
  ) content_list, nob_id)

let make_notebook nob_txts (nob_editors, nob_id) =
  List.append [Page.Text (List.nth nob_txts (nob_id - 1))] nob_editors

let () =
  let couple1 = editors_of_contents Default_contents.contents_of_notebook1 1 in
  let couple2 = editors_of_contents Default_contents.contents_of_notebook2 2 in
  let notebook1 = make_notebook Default_contents.txts_of_notebooks couple1 in
  let notebook2 = make_notebook Default_contents.txts_of_notebooks couple2 in
  let notebooks = [notebook1; notebook2] in
  List.iter (fun nob -> Page.display_notebook nob) notebooks(*;

  let s = Json.output notebook1 in
  print_endline (Js.to_string s);
  let notebook1bis : Page.notebook = Json.unsafe_input s in
  print_endline (Js.to_string (Json.output notebook1bis))*)
