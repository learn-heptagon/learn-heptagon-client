open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

let by_id s = Dom_html.getElementById s
let of_node = Tyxml_js.To_dom.of_node

open Js_of_ocaml_lwt
let (let*) = Lwt.bind

let clear_children div =
  let children = Dom.list_of_nodeList div##.childNodes in
  List.iter (fun n -> Dom.removeChild div n) children

let set_label s =
  let div = by_id "login-label" in
  clear_children div;
  Dom.appendChild div (of_node (T.txt s))

let set_title s =
  let div = by_id "login-title" in
  clear_children div;
  Dom.appendChild div (of_node (T.txt s))

let set_switch s f =
  let div = by_id "login-switch" in
  clear_children div;
  Dom.appendChild div (of_node (T.txt s));
  div##.onclick := Dom.handler (fun _ -> f (); Js._true)

let set_submit f =
  let form : Dom_html.formElement Js.t = Js.Unsafe.coerce (by_id "login-form") in
  let inp : Dom_html.inputElement Js.t = Js.Unsafe.coerce (by_id "login-input") in
  form##.onsubmit := Dom.handler
                       (fun ev ->
                         ignore ((Js.Unsafe.coerce ev)##preventDefault ());
                         f (Js.to_string inp##.value); Js._true)

let set_error s =
  let div = by_id "login-error" in
  clear_children div;
  Dom.appendChild div (of_node (T.txt s))

(* Store token and username in localStorage *)
let store_user_info ~token ~username =
  Js.Optdef.iter Dom_html.window##.localStorage (fun stor ->
    stor##setItem (Js.string "user_token") (Js.string token);
    stor##setItem (Js.string "username") (Js.string username)
  )

let load_main_page () =
  Url.Current.set (Option.get (Url.url_of_string (User.server_url "index.html")))

let try_login token =
  let* valid_user = User.get_user ~token () in
  (match valid_user with
   | Some json ->
      let username =
        match Yojson.Safe.Util.member "username" json with
        | `String u -> u
        | _ -> ""
      in
      store_user_info ~token ~username;
      load_main_page ()
   | None -> set_error "Invalid token: User not found"
  );
  Lwt.return ()

let try_create_account username =
  let* result = User.create_user ~username in
  (match result with
   | Some (token, username) ->
      store_user_info ~token ~username;
      load_main_page ()
   | None -> set_error "Username already exists");
  Lwt.return ()

let rec display_login () =
  set_title "Login";
  set_error "";
  set_label "Token:";
  set_submit try_login;
  set_switch "Create account" display_create_account

and display_create_account () =
  set_title "Create account";
  set_error "";
  set_label "Username:";
  set_submit try_create_account;
  set_switch "Login" display_login

let () =
  display_login ()
