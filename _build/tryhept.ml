open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

open Notebook

let () =
  List.iteri (fun i nob ->
    let div_id = "nob-button-" ^ (string_of_int (i + 1)) in
    let nob_button = T.(li ~a:[] [button ~a:[a_id div_id] [txt ("Open " ^ nob.title)]]) in
    Dom.appendChild (Dom_html.getElementById "nob-buttons") (Tyxml_js.To_dom.of_node nob_button);
    let div = Dom_html.getElementById div_id in
    div##.onclick := Dom_html.handler (fun _ ->
      let children = Dom.list_of_nodeList Page.container##.childNodes in
      List.iter (fun n -> Dom.removeChild Page.container n) children;
      Page.display_notebook_cells nob Page.container;
      Dom.appendChild Page.container (Tyxml_js.To_dom.of_node Page.buttons);
      Js._true)
  ) Examples.my_notebooks(*;
  let s = Json.output notebook1 in
  print_endline (Js.to_string s);
  let notebook1bis : Page.notebook = Json.unsafe_input s in
  print_endline (Js.to_string (Json.output notebook1bis))*)
