open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

open Notebook
open Page

open Js_of_ocaml_lwt
let (let*) = Lwt.bind

type container_ids = {
  editor_div_id : string;
  wrapper_div_id : string;
  chronos_div_id : string;
  interp_div_id : string;
  console_div_id : string;
}

let string_of_mls_program prog =
  Kind2_printer.print_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let highlight_line editor line is_valid =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "highlightLine") [| Js.Unsafe.inject editor; Js.Unsafe.inject line; Js.Unsafe.inject is_valid |])

let clear_all_highlights editor =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "clearAllHighlights") [| Js.Unsafe.inject editor |])

let disable_button btn =
  ignore (btn##.classList##add (Js.string "pressed"));
  btn##.disabled := Js._true

let release_button btn =
  ignore (btn##.classList##remove (Js.string "pressed"));
  btn##.disabled := Js._false

let compile_editor_code title editor (ids : container_ids) =
  Sys_js.set_channel_flusher stderr (fun e -> print_error ids.console_div_id editor e);
  reset_editor ids.console_div_id editor;
  clear_div ids.wrapper_div_id;
  clear_div ids.chronos_div_id;
  try
    let modname = Compil.prepare_module () in
    let source_code = Ace.get_contents editor in
    let p = Compil.parse_program modname source_code in
    let (mls_program, obc_program) = Compil.compile_program modname p in
    Obc_printer.print stdout obc_program;
    Interp.load_interp ids.console_div_id ids.interp_div_id obc_program (Interp.interpreter_of_example title obc_program);

    let mls_string = string_of_mls_program mls_program in
    print_endline mls_string;

    let objs_lines = Verify.get_objectives_lines mls_program in

    if objs_lines <> [] then (
      let spinner = Dom_html.createSpan Dom_html.document in
      spinner##.className := Js.string "spinner hidden";

      let verify_button =
        T.(button ~a:[
          a_onclick (fun ev ->
            Console.clear ids.console_div_id;
            let btn = Js.Opt.get (Dom_html.CoerceTo.button (Dom_html.eventTarget ev)) (fun () -> assert false) in
            disable_button btn;
            spinner##.classList##remove (Js.string "hidden");

            (* Verify.do_send_verify mls_string; *)
            Lwt.async (fun () ->
              let* props_info = Verify.get_properties_info mls_string in

              if props_info <> [] then
                (try
                  List.iter2 (fun obj_line (_, valid, counterexamples) ->
                    highlight_line editor.editor obj_line valid;

                    let chrono_div_id = ids.chronos_div_id ^ "-obj-" ^ (string_of_int obj_line) in
                    let chrono_div = Dom_html.createDiv Dom_html.document in
                    chrono_div##.id := Js.string chrono_div_id;
                    Dom.appendChild (by_id ids.chronos_div_id) chrono_div;

                    (match counterexamples with
                      | Some ce when not valid ->
                        List.iter (fun (block_name, streams) ->
                          Printf.printf "Invalid property at line %d (node '%s').\nCounterexample(s):\n" obj_line block_name;
                          List.iter (fun (name, values) ->
                            Printf.printf "%s = [%s]\n" name (String.concat ", " values)
                          ) streams
                        ) ce
                      | _ -> ())
                  ) objs_lines props_info
                with _ -> Console.error ids.console_div_id "Kind2 parse error (Should not happen, call the teacher)");

              spinner##.classList##add (Js.string "hidden");
              release_button btn;
              Lwt.return ()
            );
            true)
        ] [txt "Verify properties"])
      in
      let wrapper = by_id ids.wrapper_div_id in
      Dom.appendChild wrapper (of_node verify_button);
      Dom.appendChild wrapper spinner
    );
  with _ ->
    clear_div ids.interp_div_id

let make_container_ids id = {
  editor_div_id = "editor-" ^ id;
  wrapper_div_id = "wrapper-" ^ id;
  chronos_div_id = "chronos-" ^ id;
  interp_div_id = "interp-" ^ id;
  console_div_id = "console-" ^ id;
}

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
      | Html s ->
        let div = Tyxml_js.To_dom.of_element (T.(div ~a:[a_class ["notebook-text"]] [])) in
        (* let div = Dom_html.createDiv Dom_html.window##.document in *)
        div##.innerHTML := Js.string s;

        Dom.appendChild container div
      | Heading s ->
        let div = T.(h2 ~a:[a_class ["notebook-heading"]] [txt s]) in
        Dom.appendChild container (of_node div)
      | Editor ed ->
        let ids = make_container_ids (string_of_int ed.editor_id) in
        let editor_div = T.(div ~a:[a_id ids.editor_div_id; a_class ["editor"; "notebook-editor"]][])
        and wrapper_div = T.(div ~a:[a_id ids.wrapper_div_id; a_class ["wrapper"]][])
        and chronos_div = T.(div ~a:[a_id ids.chronos_div_id; a_class ["chronos"]][])
        and interp_div = T.(div ~a:[a_id ids.interp_div_id; a_class ["interp"]][])
        and console_div = T.(ul ~a:[a_id ids.console_div_id; a_class ["console"]][]) in

        let div = T.(div ~a:[a_class ["container"]]
          [ editor_div;
            wrapper_div;
            chronos_div;
            interp_div;
            console_div]) in
        Dom.appendChild container (of_node div);

        let editor_struct =
          Ace.({
            editor_div = by_id ids.editor_div_id;
            editor = Ace.edit (by_id ids.editor_div_id);
            marks = [];
            keybinding_menu = false
          }) in
        Ace.set_mode editor_struct "ace/mode/lustre";
        Ace.set_tab_size editor_struct 2;
        let my_editor = editor_struct.editor in
        let stored_content = get_content_from_storage ed.editor_id ed.editor_content in
        my_editor##setValue (Js.string stored_content);

        compile_editor_code nob.title editor_struct ids;
        my_editor##on (Js.string "change") (fun () ->
          Console.clear main_console_id;
          let content = Js.to_string (my_editor##getValue) in
          ed.editor_content <- content;
          Js.Optdef.iter Dom_html.window##.localStorage (fun stor ->
            let key = "editor_" ^ (string_of_int ed.editor_id) in
            stor##setItem (Js.string key) (Js.string content)
          );
          clear_all_highlights my_editor;
          compile_editor_code nob.title editor_struct ids
        );

        set_editor_height my_editor;
        clear_editor_selection my_editor
  ) nob.cells

let display_first my_nobs default_nob =
  match get_title_from_storage () with
  | Some title ->
    (match List.find_opt (fun nob -> nob.title = title) my_nobs with
     | Some nob -> display_notebook_cells nob
     | None -> display_notebook_cells default_nob)
  | None -> display_notebook_cells default_nob

let generate_navbar my_nobs =
  List.iteri (fun i nob ->
    let div_id = "nob-button-" ^ (string_of_int (i + 1)) in
    let nob_button = T.(li ~a:[] [button ~a:[a_id div_id] [txt ("Open " ^ nob.title)]]) in
    Dom.appendChild (by_id "nob-buttons") (of_node nob_button);
    let div = by_id div_id in
    div##.onclick := Dom_html.handler (fun _ ->
      Console.clear main_console_id;
      display_notebook_cells nob;
      Js._true)
  ) my_nobs;
  Dom.appendChild (by_id "save-button") (of_node save_button);
  Dom.appendChild (by_id "load-button") (of_node load_button)

let download_pervasives () =
  let outf = open_out_bin "pervasives.epci" in
  List.iter (output_byte outf) Pervasives.pervasives;
  close_out outf

let download_mathlib () =
  let outf = open_out_bin "mathlib.epci" in
  List.iter (output_byte outf) Mathlib.mathlib;
  close_out outf

let () =
  download_pervasives ();
  download_mathlib ();
  display_first Notebooks.notebooks (List.hd Notebooks.notebooks);
  generate_navbar Notebooks.notebooks
