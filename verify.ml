open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind

(** URL that the programs to verify should be sent to *)
let kind2_url =
  Option.get (Url.url_of_string (Printf.sprintf "%sverify" Url.Current.as_string))

(** Send a verification request for [prog] to the kind2 server, and handle the result *)
let send_verify prog =
  let content = Yojson.Safe.to_string (`Assoc [("prog", `String prog)]) in
  (* Send the request *)
  let* res = XmlHttpRequest.perform ~contents:(`String content) kind2_url in
  (* Handle the response *)
  Lwt.return () (* TODO *)

(** Wrapper for Lwt async stuff *)
let do_send_verify prog =
  Lwt.async (fun _ -> send_verify prog)
