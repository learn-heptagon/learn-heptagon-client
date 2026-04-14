open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5
open Ezjs_ace

open Notebook
open Page

open Js_of_ocaml_lwt
let (let*) = Lwt.bind

let debug_mode =
  let args = Url.Current.arguments in
  match List.assoc_opt "debug" args with
  | Some "true" -> true
  | _ -> false

let string_of_mls_program prog =
  Mls_printer.print_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let kind2_string_of_mls_program prog =
  Kind2_printer.print_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let string_of_obc_program prog =
  Obc_printer.print_prog Format.str_formatter prog;
  Format.flush_str_formatter ()

let js_string_of_obc_program prog =
  let prog = Obc2javascript.program prog in
  Javascript_printer.program Format.str_formatter prog;
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

let display_simulate_results editor ids title obc_program =
  try
    Interp.load_interp ids.console_div_id ids.result_div_id obc_program (Interp.interpreter_of_example title obc_program)
  with _ -> clear_div ids.result_div_id

let spinner = of_node T.(span ~a:[a_class ["spinner"]] [])

let display_verify_results editor ids kind2_string row_offset objectives =
  clear_div ids.result_div_id;
  Console.clear ids.console_div_id;

  let div = by_id ids.result_div_id in
  Dom.appendChild div spinner;

  Lwt.async (fun () ->
    let* props_info = Verify.get_properties_info kind2_string in
    Page.clear_div ids.result_div_id;
    if props_info <> [] then
      (try
        List.iter2 (fun obj_line (_, valid, counterexamples) ->
          highlight_line Ace.(editor.editor) (obj_line + row_offset) valid;
          (match counterexamples with
            | Some ce when not valid ->
              let chrono_div_id = ids.result_div_id ^ "-obj-" ^ (string_of_int obj_line) in
              show_static_chronogram chrono_div_id (obj_line + row_offset) ce ids
            | _ -> ());
        ) objectives props_info;
        if List.for_all (fun (_, valid, _) -> valid) props_info then
          let oktext = T.(p ~a:[a_class ["valid-decoration"]] [txt "All properties are valid :)"]) in
          Dom.appendChild div (of_node oktext)
       with _ -> Console.error ids.console_div_id "Kind2 parse error (Should not happen, call the teacher).");

    Lwt.return ()
  )

let display_autocorrect_results ids title mls_prog =
  clear_div ids.result_div_id;
  Console.clear ids.console_div_id;

  let div = by_id ids.result_div_id in
  Dom.appendChild div spinner;

  Lwt.async (fun () ->
    let open Autocorrect in
    let* res = autocorrect title mls_prog in
    Page.clear_div ids.result_div_id;
    let resnode =
      match res with
      | Valid ->
        T.(p ~a:[a_class ["valid-decoration"]] [txt "The implementation looks correct :)"])
      | Falsifiable ce ->
        T.(div ~a:[a_class ["invalid-decoration"]] [
             (p [txt "The implementation is incorrect, here is a counterexample:"]);
             mk_static_chronogram ce
        ])
      | Unknown ->
        T.(p ~a:[a_class ["unknown-decoration"]] [txt "Could not decide if the program is correct."])
      | Error msg ->
        T.(p ~a:[a_class ["error-decoration"]] [txt (Printf.sprintf "Autocorrect failed: %s." msg)])
    in
    Dom.appendChild (by_id ids.result_div_id) (of_node resnode);
    Lwt.return ()
  )

let display_output ids content =
  clear_div ids.result_div_id;
  let divid = ids.result_div_id^"-editor" in
  let div = of_node (T.(div ~a:[a_id divid] [])) in
  Dom.appendChild (by_id ids.result_div_id) div;
  let ed = Page.create_readonly_editor divid "ace/mode/lustre" in
  Ace.set_contents ed content;
  set_editor_height ed.editor

let compile_editor_code editor has_autocorrect aceeditor (ids : container_ids) deps =
  let deps_code = String.concat "\n" (List.filter_map (fun ed -> if ed.editor_compiles then Some ed.editor_content else None) deps) in
  let deps_length = List.length (String.split_on_char '\n' deps_code) - 1 in

  Sys_js.set_channel_flusher stderr (fun e -> print_error ids.console_div_id aceeditor (-deps_length) e);
  editor.editor_compiles <- false;
  reset_editor ids.console_div_id aceeditor;

  let reset_div id =
    clear_div id; (by_id id)##.classList##remove(Js.string "hidden")
  in
  reset_div ids.wrapper_div_id;
  reset_div ids.result_div_id;

  (try
     let modname = Compil.prepare_module () in
     let source_code = Ace.get_contents aceeditor in
     let p = Compil.parse_program modname (deps_code^source_code) in
     let (mls_program, obc_program) = Compil.compile_program modname p in
     (* Obc_printer.print stdout obc_program; *)
     editor.editor_compiles <- true;

     let objectives = Verify.get_objectives_lines mls_program in

     let wrapper = by_id ids.wrapper_div_id in

     let simul_button = Tyxml_js.To_dom.of_button (T.(button [txt "Simulate"])) in
     let verify_button = Tyxml_js.To_dom.of_button (T.(button [txt "Verify properties"])) in
     let autocorrect_button = Tyxml_js.To_dom.of_button (T.(button [txt "Autocorrect"])) in
     let minils_button = Tyxml_js.To_dom.of_button (T.(button [txt "Minils IR"])) in
     let obc_button = Tyxml_js.To_dom.of_button (T.(button [txt "Obc IR"])) in
     let js_button = Tyxml_js.To_dom.of_button (T.(button [txt "JS output"])) in
     let kind2_button = Tyxml_js.To_dom.of_button (T.(button [txt "Kind2 output"])) in

     let modes = [Simulate; Verify; Autocorrect; Minils; Obc; Js; Kind2] in

     let button_of_mode = function
       | Simulate -> simul_button
       | Verify -> verify_button
       | Autocorrect -> autocorrect_button
       | Minils -> minils_button
       | Obc -> obc_button
       | Js -> js_button
       | Kind2 -> kind2_button
     in

     let switch_mode () =
       let selected_class = Js.string "selected" in
       List.iter (fun m -> (button_of_mode m)##.classList##remove selected_class) modes;
       (button_of_mode ids.current_mode)##.classList##add selected_class;
       match ids.current_mode with
       | Simulate ->
          display_simulate_results aceeditor ids editor.editor_title obc_program
       | Verify ->
          display_verify_results aceeditor ids (kind2_string_of_mls_program mls_program) (-deps_length) objectives
       | Autocorrect ->
          display_autocorrect_results ids editor.editor_title mls_program
       | Minils ->
          display_output ids (string_of_mls_program mls_program)
       | Obc ->
          display_output ids (string_of_obc_program obc_program)
       | Js ->
          display_output ids (js_string_of_obc_program obc_program)
       | Kind2 ->
          display_output ids (kind2_string_of_mls_program mls_program)
     in

     List.iter (fun m ->
         (button_of_mode m)##.onclick :=
           Dom_html.handler (fun _ ->
               ids.current_mode <- m;
               switch_mode ();
               Js.bool true
             )
       ) modes;

     Dom.appendChild wrapper simul_button;
     if objectives <> [] then Dom.appendChild wrapper verify_button;
     (* Switch back to simulate mode on edit to avoid overloading server *)
     if ids.current_mode = Verify || ids.current_mode = Autocorrect then ids.current_mode <- Simulate;

     if has_autocorrect then (
       Dom.appendChild wrapper autocorrect_button
     );

     if debug_mode then (
       Dom.appendChild wrapper minils_button;
       Dom.appendChild wrapper obc_button;
       Dom.appendChild wrapper js_button;
       Dom.appendChild wrapper kind2_button
     );

     switch_mode ();

   with e -> (
     (by_id ids.wrapper_div_id)##.classList##add (Js.string "hidden");
     (by_id ids.result_div_id)##.classList##add (Js.string "hidden");
     if debug_mode then (
       prerr_endline (Printexc.to_string e);
       Printexc.print_backtrace stderr
     )
   ));

  (* Run callbacks set by other editors *)
  List.iter (fun c -> c editor.editor_compiles) editor.editor_callbacks

let make_container_ids id = {
  editor_div_id = "editor-" ^ id;
  wrapper_div_id = "wrapper-" ^ id;
  result_div_id = "result-" ^ id;
  console_div_id = "console-" ^ id;
  current_mode = Simulate;
}

(* Retrieve user info from localStorage *)
let get_stored_user_info () =
  let stor = Js.Optdef.get Dom_html.window##.localStorage (fun () -> assert false) in
  match stor##getItem (Js.string "user_token"), stor##getItem (Js.string "username") with
    | js_token, js_username when Js.Opt.test js_token && Js.Opt.test js_username ->
      Some (Js.to_string (Js.Opt.get js_token (fun () -> Js.string "")),
            Js.to_string (Js.Opt.get js_username (fun () -> Js.string "")))
    | _ -> None

(* Remove user info *)
let remove_user_info () =
  Js.Optdef.iter Dom_html.window##.localStorage (fun stor ->
    stor##removeItem (Js.string "user_token");
    stor##removeItem (Js.string "username")
  )

let disconnect_button =
  T.(button ~a:[
    a_onclick (fun _ ->
      remove_user_info ();
      Dom_html.window##.location##reload;
      true)]
    [txt "Disconnect"])

let go_to_login () =
  Url.Current.set (Option.get (Url.url_of_string (User.server_url "login.html")));
  Lwt.return None

(* Ensure a valid user info is available *)
let ensure_user_info () =
  match get_stored_user_info () with
    | Some (token, username) ->
      let* valid_user = User.get_user ~token () in
      (match valid_user with
        | Some _ -> Lwt.return (Some (token, username))
        | None -> go_to_login ())
    | None -> go_to_login ()

(** Find editor with title [title] in notebook [nob] *)
let find_editor nob title =
  match List.filter_map (function Editor ed when ed.editor_title = title -> Some ed | _ -> None) nob.cells with
  | ed::_ -> ed
  | _ -> failwith (Printf.sprintf "Editor %s not found" title)

(** Add .lus to the end of [s] *)
let add_suffix s = s^".lus"

(** Add [f] to the callbacks of editor [ed] *)
let add_callback ed f =
  ed.editor_callbacks <- f::ed.editor_callbacks

(** Create the text to display a dependency to [ed], along with the callbacks to change its style *)
let create_dep_text this_id ed =
  let node = Tyxml_js.To_dom.of_a T.(a ~a:[a_class ["editor-dependency"]; a_href ("#"^ed.editor_title)] [T.txt (add_suffix ed.editor_title)]) in
  let callback b =
    if b then node##.classList##remove (Js.string "invalid")
    else node##.classList##add (Js.string "invalid")
  in
  callback ed.editor_compiles; (* for initialisation *)
  add_callback ed callback;
  node

let display_notebook_cells nob =
  current_notebook := Some nob;
  save_title_in_storage nob.title;

  Lwt.async (fun () -> SaveNotebook.get_notebook nob.title);

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
        let anchor = T.(a ~a:[a_id ed.editor_title; a_class ["anchor"]] [])
        and preamble_div = T.(div ~a:[][])
        and editor_div = T.(div ~a:[a_id ids.editor_div_id; a_class ["editor"; "notebook-editor"]][])
        and wrapper_div = T.(div ~a:[a_id ids.wrapper_div_id; a_class ["wrapper"]][])
        and result_div = T.(div ~a:[a_id ids.result_div_id; a_class ["result"]][])
        and console_div = T.(ul ~a:[a_id ids.console_div_id; a_class ["console"]][]) in

        (* Create container *)
        let div =
          of_node
            (T.(div ~a:[a_class ["container"]]
                  [ anchor;
                    preamble_div;
                    editor_div;
                    wrapper_div;
                    result_div;
                    console_div])) in
        Dom.appendChild container div;

        (* Preamble: name *)
        let name = T.(p [txt "File: "; txt (add_suffix ed.editor_title)]) in
        Dom.appendChild (of_node preamble_div) (of_node name);

        (* Preamble: dependencies *)
        let dep_editors = List.map (find_editor nob) ed.editor_depends in
        if (ed.editor_depends <> []) then (
          let dep_texts = List.map (create_dep_text ed.editor_id) dep_editors in
          let span = of_node T.(p [T.txt "Depends on: "]) in
          Dom.appendChild (of_node preamble_div) span;
          List.iter (Dom.appendChild span) dep_texts
        );

        (* Append actual editor *)
        let editor_struct = Page.create_editor ids.editor_div_id in
        Ace.set_mode editor_struct "ace/mode/lustre";
        let ace_editor = editor_struct.editor in
        ace_editor##setValue (Js.string ed.editor_content);
        set_editor_height ace_editor;

        if (nob.title = "Scratchpad") then set_editor_min_height ace_editor;

        (* Recompilation function, to be called... *)
        let compfun () =
          clear_all_highlights ace_editor;
          compile_editor_code ed nob.has_autocorrect editor_struct ids dep_editors
        in

        (* (1) at initialisation *)
        compfun ();
        (* (2) when this editor changes *)
        ace_editor##on (Js.string "change") (fun () ->
            Console.clear main_console_id;
            let content = Js.to_string (ace_editor##getValue) in
            ed.editor_content <- content;
            if !SaveNotebook.ready_to_save then
              Lwt.async (fun () -> SaveNotebook.save_notebook (Option.get !current_notebook));
            compfun ()
          );
        (* (3) when another editor this one depends on recompiles *)
        List.iter (fun ed -> add_callback ed (fun _ -> compfun ())) dep_editors;

        clear_editor_selection ace_editor
  ) nob.cells

let display_first my_nobs default_nob =
  match get_title_from_storage () with
  | Some title ->
    (match List.find_opt (fun nob -> nob.title = title) my_nobs with
     | Some nob -> display_notebook_cells nob
     | None -> display_notebook_cells default_nob)
  | None -> display_notebook_cells default_nob

let generate_navbar my_nobs (token, username) =
  (* Generate buttons for each notebook *)
  List.iteri (fun i nob ->
    let div_id = "nob-button-" ^ (string_of_int (i + 1)) in
    let nob_button = T.(li ~a:[] [button ~a:[a_id div_id] [txt nob.title]]) in
    Dom.appendChild (by_id "nob-buttons") (of_node nob_button);
    let div = by_id div_id in
    div##.onclick := Dom_html.handler (fun _ ->
      Console.clear main_console_id;
      (* Lwt.async save_notebook; *)
      display_notebook_cells nob;
      Js._true)
  ) my_nobs;

  (* Download and upload buttons *)
  Dom.appendChild (by_id "download-button") (of_node (download_button SaveNotebook.download_file));
  Dom.appendChild (by_id "upload-button") (of_node (upload_button SaveNotebook.upload_file));

  (* User information *)
  let user_li = T.(span [txt ("Username: " ^ username ^ " | Token: " ^ token)]) in
  Dom.appendChild (by_id "user-info") (of_node user_li);

  (* Disconnect button *)
  Dom.appendChild (by_id "disconnect-button") (of_node disconnect_button)

let download_epci fname src =
  let outf = open_out_bin fname in
  List.iter (output_byte outf) src;
  close_out outf

let () =
  Printexc.record_backtrace debug_mode;
  Lwt.async (fun () ->
    let* uinfo = ensure_user_info () in
    (match uinfo with
     | Some (token, username) ->
       display_first Notebooks.notebooks (List.hd Notebooks.notebooks);
       generate_navbar Notebooks.notebooks (token, username)
     | None -> ());
    Lwt.return ()
  );
  download_epci "pervasives.epci" Pervasives.pervasives;
  download_epci "mathlib.epci" Mathlib.mathlib;
  download_epci "fftnative.epci" Fftnative.fftnative
