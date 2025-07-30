open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

open Notebook

let current_notebook = ref None

let set_editor_height editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "setEditorHeight") [|Js.Unsafe.inject editor|])

let clear_editor_selection editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "clearEditorSelection") [|Js.Unsafe.inject editor|])

let container = Dom_html.getElementById "container"

let display_notebook_cells nob container =
  current_notebook := Some nob;
  let children = Dom.list_of_nodeList container##.childNodes in
  List.iter (fun n -> Dom.removeChild container n) children;
  List.iter (fun nob_cell ->
    match nob_cell with
      | Text s ->
        let div = T.(div [txt s]) in
        Dom.appendChild container (Tyxml_js.To_dom.of_node div)
      | Editor ed ->
        let div_id = "editor-" ^ (string_of_int ed.editor_id) in
        let div = T.(div ~a:[a_id div_id; a_class ["editor"]][]) in
        Dom.appendChild container (Tyxml_js.To_dom.of_node div);
        let editor_struct =
          Ace.({
            editor_div = Dom_html.getElementById div_id;
            editor = Ace.edit (Dom_html.getElementById div_id);
            marks = [];
            keybinding_menu = false
          }) in
        Ace.set_mode editor_struct "ace/mode/lustre";
        Ace.set_tab_size editor_struct 2;
        let my_editor = editor_struct.editor in
        my_editor##setValue (Js.string ed.editor_content);
        set_editor_height my_editor;
        clear_editor_selection my_editor
  ) nob.cells

let save_file filename content =
  let blob = File.blob_from_string ~contentType:"text/plain" content in
  let url = Dom_html.window##._URL##createObjectURL blob in
  let a = Dom_html.createA Dom_html.document in
  a##.href := url;
  a##.download := Js.string filename;
  Dom.appendChild Dom_html.document##.body a;
  a##click;
  Dom.removeChild Dom_html.document##.body a;
  Dom_html.window##._URL##revokeObjectURL url

let load_file ev =
  let element = Dom_html.eventTarget ev in
  let input_element = Js.Opt.to_option (Dom_html.CoerceTo.input element) in
  match input_element with
    | Some input ->
      let files_opt = Js.Optdef.to_option input##.files in
      (match files_opt with
        | Some files when files##.length > 0 ->
          let reader = new%js File.fileReader in
          reader##.onload :=
            Dom.handler (fun _ ->
              let content = Js.Opt.get (File.CoerceTo.string reader##.result) (fun () -> assert false) in
              let (title, contents) : (string * string list) = Json.unsafe_input content in
              (match !current_notebook with
                | Some nob when nob.title = title ->
                  let editors = List.filter_map (fun nob_cell ->
                    match nob_cell with
                      | Editor ed -> Some ed
                      | _ -> None
                  ) nob.cells
                  in
                  if List.length editors = List.length contents then (
                    List.iter2 (fun ed c ->
                      let my_editor = Ace.edit (Dom_html.getElementById ("editor-" ^ string_of_int ed.editor_id)) in
                      my_editor##setValue (Js.string c);
                      set_editor_height my_editor;
                      clear_editor_selection my_editor
                    ) editors contents
                  ) else
                    print_endline "Mismatch between number of editors and content's length"
                | Some _ -> print_endline "Mismatch between notebooks' titles"
                | None -> ());
              Js._false
            );
          reader##readAsText (Js.Opt.get (files##item 0) (fun () -> assert false));
          true
        | _ -> false)
    | _ -> false

let save_button =
  T.(button ~a:[
    a_onclick (fun _ ->
      match !current_notebook with
        | Some nob ->
          let content =
            List.fold_right (fun nob_cell acc ->
              match nob_cell with
                | Editor ed -> ed.editor_content :: acc
                | _ -> acc
            ) nob.cells []
          in
          let json = Json.output (nob.title, content) in
          save_file (nob.title ^ ".json") (Js.to_string json);
          true
        | None -> false)]
  [txt "Save notebook"])

let load_button =
  let input_id = "input" in
  T.(div [
      input ~a:[
        a_input_type `File;
        a_onchange (fun ev -> load_file ev);
        a_id input_id;
        a_style "display: none;"]
      ();
      label ~a:[
        a_label_for input_id]
      [txt "Load notebook"]])
