open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

type editor_data = {
  editor_id : int;
  editor_title : string;
  editor_content : string;
}

type cell =
  | Text of string
  | Editor of editor_data

type notebook = cell list

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
          let reader = Js.Unsafe.new_obj (Js.Unsafe.js_expr "FileReader") [||] in
          reader##.onload := Dom.handler (fun _ev ->
            let content = Js.Opt.get reader##.result (fun () -> assert false) in
            let my_notebook : notebook = Json.unsafe_input content in
            print_endline (Js.to_string content);
            Js._false
          );
          true
        | _ -> false)
    | _ -> false

let current_editors : (unit Ace.editor * unit option) Ace_types.editor Js_of_ocaml.Js.t list ref = ref []

(* TODO: implement a function that takes a variable of type editor_contents (editor_content list) and that assigns its elements to the editor_content fields of a notebook *)

(*for download + cookies (session storage)*)
let get_editor_contents (u : unit) : string list = []

(*for upload + load from cookies*)
let set_editor_contents (l : string list) : unit = ()

let download_button =
  T.(button ~a:[
    a_onclick (fun _ ->
      match !current_editors with
        | e :: _ ->
          let content = Js.to_string (e##getValue) in
          save_file "notebook_id.json" content;
          true
        | [] -> false)]
  [txt "Download file"])

let upload_button =
  T.(input ~a:[
    a_input_type `File;
    a_onchange (fun ev ->
      load_file ev)]
  ())

let set_editor_height editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "setEditorHeight") [|Js.Unsafe.inject editor|])

let clear_editor_selection editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "clearEditorSelection") [|Js.Unsafe.inject editor|])

let display_notebook nob =
  List.iter (fun cell ->
    match cell with
      | Editor ed ->
        let div_id = "editor-id-" ^ (string_of_int ed.editor_id) in
        let div = T.(div ~a:[a_id div_id; a_class ["editor"]][]) in
        Dom.appendChild Dom_html.document##.body (Tyxml_js.To_dom.of_node div);
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
        clear_editor_selection my_editor;
        current_editors := my_editor :: !current_editors
      | Text s ->
        let div = T.(div [txt s]) in
        Dom.appendChild Dom_html.document##.body (Tyxml_js.To_dom.of_node div)
  ) nob;
  current_editors := List.rev !current_editors;
  Dom.appendChild Dom_html.document##.body (Tyxml_js.To_dom.of_node download_button);
  Dom.appendChild Dom_html.document##.body (Tyxml_js.To_dom.of_node upload_button)
