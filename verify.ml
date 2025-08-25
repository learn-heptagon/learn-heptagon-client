open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind

(** URL that the programs to verify should be sent to *)
let kind2_url =
  let port = Option.fold ~none:"" ~some:(Printf.sprintf ":%d") Url.Current.port in
  let url = Printf.sprintf "%s//%s%s/verify" Url.Current.protocol Url.Current.host port in
  Option.get (Url.url_of_string url)

(** Send a verification request for [prog] to the kind2 server, and handle the result *)
let send_verify prog =
  let content = Yojson.Safe.to_string (`Assoc [("prog", `String prog)]) in
  (* Send the request *)
  XmlHttpRequest.perform
    ~content_type:"application/json"
    ~contents:(`String content) kind2_url

let format_counterexamples (json : Yojson.Safe.t) : (string * (string * string list) list) list =
  match json with
    | `List ce_list ->
      List.filter_map (fun blk ->
        match blk with
          | `Assoc blk_assoc ->
            let block_name =
              match List.assoc_opt "name" blk_assoc with
                | Some (`String n) -> n
                | _ -> "?"
            in
            let ce_list =
              match List.assoc_opt "streams" blk_assoc with
                | Some (`List streams) ->
                  List.filter_map (fun s ->
                    match s with
                      | `Assoc s_assoc ->
                        let name =
                          match List.assoc_opt "name" s_assoc with
                            | Some (`String n) -> n
                            | _ -> "?"
                        in
                        let cls =
                          match List.assoc_opt "class" s_assoc with
                            | Some (`String c) -> c
                            | _ -> "?"
                        in
                        (match List.assoc_opt "instantValues" s_assoc with
                          | Some (`List instants) ->
                            let values =
                              List.map (fun v ->
                                match v with
                                  | `List [_step; `Assoc frac] ->
                                    let num =
                                      match List.assoc_opt "num" frac with Some (`Int n) -> n | _ -> 0
                                    in
                                    let den =
                                      match List.assoc_opt "den" frac with Some (`Int d) -> d | _ -> 1
                                    in
                                    Printf.sprintf "%d/%d" num den
                                  | `List [_step; `Bool b] -> string_of_bool b
                                  | _ -> "?"
                              ) instants
                            in
                            if cls = "input" || cls = "output" then Some (name, values) else None
                          | _ -> None)
                      | _ -> None
                  ) streams
                | _ -> []
            in
            Some (block_name, ce_list)
          | _ -> None
      ) ce_list
    | _ -> []

let get_properties_info prog =
  (* Send the request *)
  let* res = send_verify prog in
  (* Handle the response *)
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
                      let line =
                        match List.assoc_opt "line" fields with
                          | Some (`Int l) -> l
                          | _ -> -1
                      in
                      let valid =
                        match List.assoc_opt "answer" fields with
                          | Some (`Assoc ans_fields) -> (
                            match List.assoc_opt "value" ans_fields with
                              | Some (`String "valid") -> true
                              | _ -> false)
                          | _ -> false
                      in
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
      with Yojson.Json_error msg ->
        prerr_endline ("JSON parsing error: " ^ msg);
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
