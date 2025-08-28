open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind


(** Client part to perform the "/verify" request **)

let current_url =
  let port = Option.fold ~none:"" ~some:(Printf.sprintf ":%d") Url.Current.port in
  Printf.sprintf "%s//%s%s" Url.Current.protocol Url.Current.host port

(** URL that the programs to verify should be sent to *)
let kind2_url =
  let url = Printf.sprintf "%s/verify" current_url in
  Option.get (Url.url_of_string url)

(** Send a verification request for [prog] to the kind2 server, and handle the result *)
let send_verify prog =
  let content = Yojson.Safe.to_string (`Assoc [("prog", `String prog)]) in
  (* Send the request *)
  XmlHttpRequest.perform
    ~content_type:"application/json"
    ~contents:(`String content) kind2_url

let format_counterexamples (json : Yojson.Safe.t) : (string * (string * string list) list) list =
  let open Kind2Json in
  let ce_list = get_list json in
  List.filter_map (fun blk ->
    let blk_assoc = get_assoc blk in
    let block_name = get_string_field "name" blk_assoc in
    let streams = get_list_field "streams" blk_assoc in
    let ce_list =
      List.filter_map (fun s ->
        let s_assoc = get_assoc s in
        let name = get_string_field "name" s_assoc in
        let cls = get_string_field "class" s_assoc in
        let instants = get_list_field "instantValues" s_assoc in
        let values =
          List.map (fun v ->
            match v with
              | `List [_step; `Assoc frac] ->
                let num = get_int_field "num" frac
                and den = get_int_field "den" frac in
                Printf.sprintf "%d/%d" num den
              | `List [_step; `Bool b] -> string_of_bool b
              | _ -> "?"
          ) instants
        in
        if cls = "input" || cls = "output" then Some (name, values) else None
      ) streams
    in
    Some (block_name, ce_list)
  ) ce_list

let get_properties_info prog =
  (* Send the request *)
  let* res = send_verify prog in
  (* Handle the response *)
  let open Kind2Json in
  match res.code with
    | 200 ->
      (try
        let json = Yojson.Safe.from_string res.content in
        let props =
          match json with
            | `List items ->
              List.filter_map
                (fun item ->
                  (match item with
                    | `Assoc fields when List.assoc_opt "objectType" fields = Some (`String "property") ->
                      let line = get_int_field "line" fields in
                      let valid = get_string_field "value" (get_assoc_field "answer" fields) in
                      let valid = (valid = "valid") in
                      let counterexamples =
                        match List.assoc_opt "counterExample" fields with
                          | Some json -> Some (format_counterexamples json)
                          | _ -> None
                      in
                      Some (line, valid, counterexamples)
                    | _ -> None)
                ) items
            | _ -> []
        in

        (* Sort the list of properties by their line number in ascending order *)
        let sorted_props = List.sort (fun (line1, _, _) (line2, _, _) -> compare line1 line2) props in

        Lwt.return sorted_props
      with
      | Yojson.Json_error msg ->
        prerr_endline ("JSON parsing error: " ^ msg);
        Lwt.return []
      | Malformed_response ->
        prerr_endline "Malformed response";
        Lwt.return [])
    | code ->
      prerr_endline (Printf.sprintf "HTTP error %d: %s" code res.content);
      Lwt.return []

let get_objectives_lines (p: Minils.program) : int list =
  let open Minils in
  List.concat_map (fun program_desc ->
    match program_desc with
      | Pnode nd ->
        (match nd.n_contract with
          | Some c -> List.map (fun o -> let Loc (p, _) = o.o_exp.w_loc in p.pos_lnum) c.c_objectives
          | None -> [])
      | _ -> []) p.p_desc

(* (\** Wrapper for Lwt async stuff *\) *)
(* let do_send_verify prog = *)
(*   Lwt.async (fun _ -> send_verify prog) *)
