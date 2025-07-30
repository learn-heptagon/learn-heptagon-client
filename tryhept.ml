open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

let display = Page.display_notebook_cells Page.container

let display_first my_nobs default_nob =
  match Page.get_title_from_storage () with
  | Some title ->
    (match List.find_opt (fun nob -> nob.Page.title = title) my_nobs with
     | Some nob -> display nob
     | None -> display default_nob)
  | None -> display default_nob

let () =
  display_first Examples.my_notebooks Examples.notebook1;
  List.iteri (fun i nob ->
    let div_id = "nob-button-" ^ (string_of_int (i + 1)) in
    let nob_button = T.(li ~a:[] [button ~a:[a_id div_id] [txt ("Open " ^ nob.Page.title)]]) in
    Dom.appendChild (Dom_html.getElementById "nob-buttons") (Tyxml_js.To_dom.of_node nob_button);
    let div = Dom_html.getElementById div_id in
    div##.onclick := Dom_html.handler (fun _ ->
      display nob;
      Js._true)
  ) Examples.my_notebooks;
  Dom.appendChild (Dom_html.getElementById "save-button") (Tyxml_js.To_dom.of_node Page.save_button);
  Dom.appendChild (Dom_html.getElementById "load-button") (Tyxml_js.To_dom.of_node Page.load_button)
