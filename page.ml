open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

open Notebook

let current_notebook = ref None

let save_title_in_storage s =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> ())
    (fun stor ->
       stor##setItem (Js.string "title") (Js.string s))

let get_title_from_storage () =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> None)
    (fun stor ->
       Js.Opt.case (stor##getItem (Js.string "title"))
         (fun () -> None)
         (fun s -> Some (Js.to_string s)))

let get_content_from_storage id default_content =
  Js.Optdef.case Dom_html.window##.localStorage
    (fun () -> default_content)
    (fun stor ->
       Js.Opt.case (stor##getItem (Js.string ("editor_" ^ string_of_int id)))
         (fun () -> default_content)
         (fun s -> Js.to_string s))

let set_editor_height editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "setEditorHeight") [|Js.Unsafe.inject editor|])

let clear_editor_selection editor =
  ignore (Js.Unsafe.fun_call(Js.Unsafe.js_expr "clearEditorSelection") [|Js.Unsafe.inject editor|])

let by_id s = Dom_html.getElementById s
let of_node = Tyxml_js.To_dom.of_node

(** Manipulate a given console *)
module Console = struct
  (** Scroll to the bottom *)
  let scroll console child =
    console##.scrollTop := child##.offsetTop

  (** Log a message *)
  let log console_id msg =
    let console = by_id console_id in
    let newLine = of_node (T.(li ~a:[a_class ["console-log"]] [txt msg])) in
    ignore (console##appendChild newLine);
    scroll console (Js.Unsafe.coerce newLine)

  (** Log an error message *)
  let error console_id msg =
    let console = by_id console_id in
    let newLine = of_node (T.(li ~a:[a_class ["console-error"]] [txt msg])) in
    ignore (console##appendChild newLine);
    scroll console (Js.Unsafe.coerce newLine)

  (** Delete all lines *)
  let clear console_id =
    let console = by_id console_id in
    while Js.to_bool console##hasChildNodes do
      Js.Opt.iter (console##.firstChild) (fun c -> Dom.removeChild console c)
    done
end

let main_console_id = "main-console"

(** Show an error with [text] at [loc] in the console as well as the editor *)
let add_error_marker (editor : unit Ace.editor) (r1, r2) (c1, c2) =
  let open Ace in let open Ace_types in
  (* Printf.fprintf stdout "%d:%d, %d:%d\n" r1 c1 r2 c2; *)
  let range = Ace.range (r1-1) c1 (r2-1) c2 in
  ignore (editor.editor##getSession##addMarker range
            (Js.string "error-marker") (Js.string "text") (Js.bool true))

let parse_loc_message text =
  let reg = Str.regexp "File \"\", line \\([0-9]+\\)-?\\([0-9]+\\)?, characters \\([0-9]+\\)-\\([0-9]+\\):" in
  if not (Str.string_match reg text 0) then invalid_arg "parse_loc_message";
  let r1 = int_of_string (Str.matched_group 1 text) in
  let r2 = try int_of_string (Str.matched_group 2 text) with _ -> r1 in
  let c1 = int_of_string (Str.matched_group 3 text) and c2 = int_of_string (Str.matched_group 4 text) in
  (r1, r2), (c1, c2)

let print_error console_div_id editor text =
  print_endline text;
  try
    let (row, col) = parse_loc_message text in
    add_error_marker editor row col
  with _ ->
    if text <> "\n" then Console.error console_div_id text

(** Reset the editor *)
let reset_editor console_div_id (editor: unit Ace.editor) =
  Ace.clear_marks editor;

  let markers = editor.editor##getSession##getMarkers (Js.bool true) in
  let markers = Js.Unsafe.global##._Object##keys markers in
  let markers = Js.to_array markers in
    Array.iter (fun m -> editor.editor##getSession##removeMarker m) markers;

  Console.clear console_div_id

let compile_editor_code console_div_id editor =
  Sys_js.set_channel_flusher stderr (fun e -> print_error console_div_id editor e);
  reset_editor console_div_id editor;
  try
    let modname = Compil.prepare_module () in
    let p = Compil.parse_program modname (Ace.get_contents editor) in
    let p = Compil.compile_program modname p in
    Obc_printer.print stdout p
  with _ -> ()

let display_notebook_cells nob =
  current_notebook := Some nob;
  save_title_in_storage nob.title;

  let container = by_id "container" in
  let children = Dom.list_of_nodeList container##.childNodes in
  List.iter (fun n -> Dom.removeChild container n) children;

  List.iter (fun nob_cell ->
    match nob_cell with
      | Text s ->
        let div = T.(div [txt s]) in
        Dom.appendChild container (of_node div)
      | Editor ed ->
        let editor_div_id = "editor-" ^ (string_of_int ed.editor_id)
        and console_div_id = "console-" ^ string_of_int ed.editor_id in

        let editor_div = T.(div ~a:[a_id editor_div_id; a_class ["editor"]][])
        and console_div = T.(ul ~a:[a_id console_div_id; a_class ["console"]][]) in

        let div = T.(div ~a:[a_class ["editor-console"]] [editor_div; console_div]) in
        Dom.appendChild container (of_node div);
        let editor_struct =
          Ace.({
            editor_div = by_id editor_div_id;
            editor = Ace.edit (by_id editor_div_id);
            marks = [];
            keybinding_menu = false
          }) in
        Ace.set_mode editor_struct "ace/mode/lustre";
        Ace.set_tab_size editor_struct 2;
        let my_editor = editor_struct.editor in
        let stored_content = get_content_from_storage ed.editor_id ed.editor_content in
        my_editor##setValue (Js.string stored_content);
        compile_editor_code console_div_id editor_struct;

        my_editor##on (Js.string "change") (fun () ->
          Console.clear main_console_id;
          let content = Js.to_string (my_editor##getValue) in
          ed.editor_content <- content;
          Js.Optdef.iter Dom_html.window##.localStorage (fun stor ->
            let key = "editor_" ^ string_of_int ed.editor_id in
            stor##setItem (Js.string key) (Js.string content)
          );
          compile_editor_code console_div_id editor_struct
        );

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
  Console.clear main_console_id;

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
                      let my_editor = Ace.edit (by_id ("editor-" ^ string_of_int ed.editor_id)) in
                      my_editor##setValue (Js.string c);
                      set_editor_height my_editor;
                      clear_editor_selection my_editor
                    ) editors contents;
                    Console.log main_console_id "The notebook has been successfully imported."
                  ) else (
                    Console.error main_console_id "Importation failed: mismatch between number of editors and content's length."
                  )
                | Some _ -> Console.error main_console_id "Importation failed: mismatch between notebooks' titles."
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
  let id = "input" in
  T.(div [
      input ~a:[
        a_input_type `File;
        a_onchange (fun ev -> load_file ev);
        a_id id;
        a_style "display: none;"]
      ();
      label ~a:[
        a_label_for id]
      [txt "Load notebook"]])
