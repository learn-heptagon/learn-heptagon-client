open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

open Notebook
open Page

open Js_of_ocaml_lwt
let (let*) = Lwt.bind

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

let display_simulate_results editor ids obc_program =
  Interp.load_interp ids.console_div_id ids.result_div_id obc_program (Interp.interpreter_of_example "" obc_program)

let display_verify_results editor ids mls_string objectives =
  clear_div ids.result_div_id;
  Console.clear ids.console_div_id;

  let spinner = Dom_html.createSpan Dom_html.document in
  spinner##.className := Js.string "spinner";
  Dom.appendChild (by_id ids.result_div_id) spinner;

  Lwt.async (fun () ->
    let* props_info = Verify.get_properties_info mls_string in
    Page.clear_div ids.result_div_id;
    if props_info <> [] then
      (try
        List.iter2 (fun obj_line (_, valid, counterexamples) ->
          highlight_line Ace.(editor.editor) obj_line valid;
          (match counterexamples with
            | Some ce when not valid ->
              let chrono_div_id = ids.result_div_id ^ "-obj-" ^ (string_of_int obj_line) in
              show_static_chronogram chrono_div_id obj_line ce ids
            | _ -> ());
        ) objectives props_info;
        if List.for_all (fun (_, valid, _) -> valid) props_info then
          let oktext = T.(p ~a:[a_class ["valid-decoration"]] [txt "All properties are valid :)"]) in
          Dom.appendChild (by_id ids.result_div_id) (of_node oktext)
       with _ -> Console.error ids.console_div_id "Kind2 parse error (Should not happen, call the teacher)");

    (* spinner##.classList##add (Js.string "hidden"); *)
    (* release_button btn; *)
    Lwt.return ()
  )

let compile_editor_code (title: string) editor (ids : container_ids) =
  Sys_js.set_channel_flusher stderr (fun e -> print_error ids.console_div_id editor e);
  reset_editor ids.console_div_id editor;

  let reset_div id =
    clear_div id; (by_id id)##.classList##remove(Js.string "hidden")
  in
  reset_div ids.wrapper_div_id;
  reset_div ids.result_div_id;

  try
    let modname = Compil.prepare_module () in
    let source_code = Ace.get_contents editor in
    let p = Compil.parse_program modname source_code in
    let (mls_program, obc_program) = Compil.compile_program modname p in
    let mls_string = string_of_mls_program mls_program in
    print_endline mls_string;
    Obc_printer.print stdout obc_program;

    let objectives = Verify.get_objectives_lines mls_program in

    let wrapper = by_id ids.wrapper_div_id in

    let simul_button = Tyxml_js.To_dom.of_button (T.(button [txt "Simulate"])) in
    let verify_button = Tyxml_js.To_dom.of_button (T.(button [txt "Verify properties"])) in

    let buttons = [simul_button; verify_button] in

    let switch_mode () =
      let selected_class = Js.string "selected" in
      List.iter (fun b -> b##.classList##remove selected_class) buttons;
      match ids.current_mode with
        | Simulate ->
          simul_button##.classList##add selected_class;
          display_simulate_results editor ids obc_program
        | Verify ->
          verify_button##.classList##add selected_class;
          display_verify_results editor ids mls_string objectives
    in

    simul_button##.onclick :=
        Dom_html.handler (fun _ ->
          ids.current_mode <- Simulate;
          switch_mode ();
          Js.bool true
        );
    verify_button##.onclick :=
        Dom_html.handler (fun _ ->
          ids.current_mode <- Verify;
          switch_mode ();
          Js.bool true
        );

    Dom.appendChild wrapper simul_button;
    if objectives <> [] then Dom.appendChild wrapper verify_button
    else if ids.current_mode = Verify then ids.current_mode <- Simulate;

    switch_mode ()
  with _ -> (
    (by_id ids.wrapper_div_id)##.classList##add (Js.string "hidden");
    (by_id ids.result_div_id)##.classList##add (Js.string "hidden")
  )

let make_container_ids id = {
  editor_div_id = "editor-" ^ id;
  wrapper_div_id = "wrapper-" ^ id;
  result_div_id = "result-" ^ id;
  console_div_id = "console-" ^ id;
  current_mode = Simulate;
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
        and result_div = T.(div ~a:[a_id ids.result_div_id; a_class ["result"]][])
        and console_div = T.(ul ~a:[a_id ids.console_div_id; a_class ["console"]][]) in

        let div = T.(div ~a:[a_class ["container"]]
          [ editor_div;
            wrapper_div;
            result_div;
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
