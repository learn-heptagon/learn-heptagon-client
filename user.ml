open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind


(** Client part to perform the "/create-user" and "/get-user" requests **)

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
      prerr_endline "User not found on the server.";
      Lwt.return_none
    | _ ->
      prerr_endline (Printf.sprintf "HTTP error %d: %s" res.code res.content);
      Lwt.return_none


(** Client part to perform the "/save-notebook" and "/get-notebook" requests **)

(* Save a notebook for a given user *)
let save_notebook ~token ~filename ~notebook_json =
  (* Build the JSON request body *)
  let assoc_body =
    `Assoc [
      ("token", `String token);
      ("notebook", `Assoc [
        ("filename", `String filename);
        ("content", notebook_json)  (* Notebook content as JSON *)
      ])
    ]
  in
  let body = Yojson.Safe.to_string assoc_body in
  (* Perform an HTTP POST request to /save-notebook *)
  let* res =
    XmlHttpRequest.perform_raw_url
      ~content_type:"application/json"
      ~contents:(`String body)
      (kind2_url "/save-notebook")
  in
  match res.code with
    | 200 -> Lwt.return_ok ()
    | _ ->
      prerr_endline (Printf.sprintf "Failed to save notebook: %d %s." res.code res.content);
      Lwt.return_error res.content

(* Retrieve a notebook for a given user *)
let get_notebook ~token ~filename =
  (* Build the JSON request body *)
  let assoc_body =
    `Assoc [
      ("token", `String token);
      ("filename", `String filename)
    ]
  in
  let body = Yojson.Safe.to_string assoc_body in
  (* Perform an HTTP POST request to /get-notebook *)
  let* res =
    XmlHttpRequest.perform_raw_url
      ~content_type:"application/json"
      ~contents:(`String body)
      (kind2_url "/get-notebook")
  in
  match res.code with
    | 200 ->
      (* Parse the returned JSON *)
      let json =
        try Yojson.Safe.from_string res.content
        with _ ->
          prerr_endline "Failed to parse notebook JSON.";
          `Null
      in
      Lwt.return_some json
    | 404 ->
      prerr_endline "Notebook not found on server.";
      Lwt.return_none
    | _ ->
      prerr_endline (Printf.sprintf "Error retrieving notebook: %d %s." res.code res.content);
      Lwt.return_none
