open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind


(** Client part to perform the "/create-user" and "/get-user" requests **)

(* Build the complete URL for a given endpoint path *)
let server_url dest =
  let port =
    match Url.Current.port with
    | Some p -> Printf.sprintf ":%d" p
    | None -> ""
  in
  let path = List.filter (function "login.html" | "index.html" -> false | _ -> true) Url.Current.path in
  let path = String.concat "/" path in
  Printf.sprintf "%s//%s%s%s/%s" Url.Current.protocol Url.Current.host port path dest

(* Create a new user with a given username (returns token + username) *)
let create_user ~username =
  let body = Yojson.Safe.to_string (`Assoc [("username", `String username)]) in
  (* Perform an HTTP POST request to /create-user *)
  let* res =
    XmlHttpRequest.perform_raw_url
      ~content_type:"application/json"
      ~contents:(`String body)
      (server_url "create-user")
  in
  match res.code with
    | 200 ->
      let json = Yojson.Safe.from_string res.content in
      let token_opt =
        match Yojson.Safe.Util.member "token" json with
        | `String t -> Some t
        | _ -> None
      in
      (match token_opt with
        | Some t -> Lwt.return_some (t, username)
        | None -> Lwt.return_none)
    | 409 ->
      prerr_endline "Username already exists.";
      Lwt.return_none
    | _ ->
      prerr_endline (Printf.sprintf "HTTP error %d: %s" res.code res.content);
      Lwt.return_none

(* Retrieve user information using token or username *)
let get_user ?token ?username () =
  let fields = [] in
  let fields =
    match token with Some t -> ("token", `String t)::fields | None -> fields
  in
  let fields =
    match username with Some u -> ("username", `String u)::fields | None -> fields
  in
  if fields = [] then begin
    prerr_endline "Must provide token or username to get user.";
    Lwt.return_none
  end else
    let body = Yojson.Safe.to_string (`Assoc fields) in
    let* res =
      XmlHttpRequest.perform_raw_url
        ~content_type:"application/json"
        ~contents:(`String body)
        (server_url "get-user")
    in
    match res.code with
      | 200 ->
        let json = Yojson.Safe.from_string res.content in
        Lwt.return_some json
    | 404 ->
          prerr_endline "User not found on server.";
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
      (server_url "save-notebook")
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
      (server_url "get-notebook")
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
