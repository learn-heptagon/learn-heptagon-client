open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind

(* Build the complete URL for a given endpoint path *)
let kind2_url path =
  Printf.sprintf "%s%s" Url.Current.as_string path

(* Create a new user (returns the token) *)
let create_user () =
  (* Perform an HTTP GET request to /create-user *)
  let* res = XmlHttpRequest.get (kind2_url "create-user") in
  match res.code with
    | 200 ->
      (* Parse the JSON response *)
      let json = Yojson.Safe.from_string res.content in
      (match Yojson.Safe.Util.member "token" json with
        | `String t ->
          (* Return the token *)
          Lwt.return_some t
        | _ ->
          (* No valid token field found *)
          Lwt.return_none)
    | _ ->
      (* Request failed *)
      prerr_endline (Printf.sprintf "HTTP error %d: %s" res.code res.content);
      Lwt.return_none

(* Retrieve user information from the server using an existing token *)
let get_user token =
  (* Build the request body: { "token": "..."} *)
  let body = Yojson.Safe.to_string (`Assoc [("token", `String token)]) in

  (* Perform an HTTP POST request to /get-user *)
  let* res =
    XmlHttpRequest.perform_raw_url
      ~content_type:"application/json"
      ~contents:(`String body)
      (kind2_url "get-user")
  in
  match res.code with
    | 200 ->
      (* Parse the response and extract the token *)
      let json = Yojson.Safe.from_string res.content in
      (match Yojson.Safe.Util.member "token" json with
        | `String t -> Lwt.return_some t
        | _ -> Lwt.return_none)
    | 404 ->
      prerr_endline "User not found on the server";
      Lwt.return_none
    | _ ->
      prerr_endline (Printf.sprintf "HTTP error %d: %s" res.code res.content);
      Lwt.return_none
