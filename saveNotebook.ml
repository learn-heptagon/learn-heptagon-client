open Js_of_ocaml
open Ezjs_ace

open Notebook
open Page

open Js_of_ocaml_lwt
let (let*) = Lwt.bind

(* Save "lock": we do not want to save the notebook if an editor changes during initial load *)
let ready_to_save = ref true

let set_contents editors contents =
  ready_to_save := false;
  List.iter2 (fun ed c ->
    let my_editor = Ace.edit (by_id ("editor-" ^ string_of_int ed.editor_id)) in
    my_editor##setValue (Js.string c);
    set_editor_height my_editor;
    clear_editor_selection my_editor
  ) editors contents;
  ready_to_save := true

(* Save notebook to the server *)
let save_notebook nob =
  (* Prepare notebook content for server *)
  let content =
    List.fold_right (fun nob_cell acc ->
      match nob_cell with
        | Editor ed -> ed.editor_content :: acc
        | _ -> acc
    ) nob.cells []
  in
  let notebook_json = `Assoc [
                        ("filename", `String nob.title);
                        ("notebook", `List (List.map (fun s -> `String s) content))
                      ] in
  let token =
    let stor = Js.Optdef.get Dom_html.window##.localStorage (fun () -> assert false) in
    Js.Opt.case (stor##getItem (Js.string "user_token"))
    (fun () -> failwith "No user token")
    (fun js -> Js.to_string js)
  in
  let body =
    Yojson.Safe.to_string
    (`Assoc [
       ("token", `String token);
       ("notebook", notebook_json)
    ])
  in
  let url =
    match Url.url_of_string (User.server_url "save-notebook") with
      | Some u -> u
      | None -> failwith "Invalid URL"
  in
  let* res = XmlHttpRequest.perform
             ~content_type:"application/json"
             ~contents:(`String body)
             url
  in
  (match res.code with
    | 200 -> (* Console.log main_console_id "Notebook saved successfully."; *) Lwt.return_unit
    | _ -> Console.error main_console_id ("Save failed: " ^ res.content); Lwt.return_unit)

(** Get notebook from server *)
let get_notebook filename =
  let token =
    let stor = Js.Optdef.get Dom_html.window##.localStorage (fun () -> assert false) in
    Js.Opt.case (stor##getItem (Js.string "user_token"))
      (fun () -> failwith "No user token")
      (fun js -> Js.to_string js)
  in
  let body =
    Yojson.Safe.to_string
      (`Assoc [
        ("token", `String token);
        ("filename", `String filename)
      ])
  in
  let url =
    match Url.url_of_string (User.server_url "get-notebook") with
    | Some u -> u
    | None -> failwith "Invalid URL"
  in
  let* res = XmlHttpRequest.perform
              ~content_type:"application/json"
              ~contents:(`String body)
              url
  in
  match res.code with
    | 200 ->
      let json = Yojson.Safe.from_string res.content in
      let contents =
        match Yojson.Safe.Util.member "notebook" json with
          | `List lst -> List.map (function `String s -> s | _ -> "") lst
          | _ -> []
      in
      (match !current_notebook with
        | Some nob ->
          let editors = List.filter_map (function Editor ed -> Some ed | _ -> None) nob.cells in
          set_contents editors contents;
          Console.log main_console_id "Notebook loaded successfully."
        | None -> ());
      Lwt.return_unit
    | 204 -> Lwt.return_unit
    | _ -> Console.error main_console_id ("Get failed: " ^ res.content); Lwt.return_unit

(** Download notebook to PC *)
let download_file filename content =
  let blob = File.blob_from_string ~contentType:"text/plain" content in
  let url = Dom_html.window##._URL##createObjectURL blob in
  let a = Dom_html.createA Dom_html.document in
  a##.href := url;
  a##.download := Js.string filename;
  Dom.appendChild Dom_html.document##.body a;
  a##click;
  Dom.removeChild Dom_html.document##.body a;
  Dom_html.window##._URL##revokeObjectURL url

(** Upload notebook from PC *)
let upload_file ev =
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
                    set_contents editors contents;
                    Console.log main_console_id "Notebook imported successfully.";
                    Lwt.async (fun () -> save_notebook nob)
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
