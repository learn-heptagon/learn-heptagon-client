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
  XmlHttpRequest.perform
    ~content_type:"application/json"
    ~contents:(`String content) kind2_url

let format_counterexample (json : Yojson.Safe.t) =
  match json with
    | `List (ce_list : Yojson.Safe.t list) ->
      let filtered_blocks =
        List.filter_map
          (fun (blk : Yojson.Safe.t) ->
            match blk with
              | `Assoc (blk_assoc : (string * Yojson.Safe.t) list) ->
                (match List.assoc_opt "streams" blk_assoc with
                  | Some (`List (streams : Yojson.Safe.t list)) ->
                    let lines =
                      List.filter_map
                        (fun (s : Yojson.Safe.t) ->
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
                              let value =
                                match List.assoc_opt "instantValues" s_assoc with
                                  | Some (`List ((`List [ _step; `Assoc frac ]) :: _)) ->
                                    let num =
                                      match List.assoc_opt "num" frac with
                                        | Some (`Int n) -> n
                                        | _ -> 0
                                    in
                                    let den =
                                      match List.assoc_opt "den" frac with
                                        | Some (`Int d) -> d
                                        | _ -> 1
                                    in
                                    Printf.sprintf "%d/%d" num den
                                  | _ -> "?"
                              in
                              if cls = "input" || cls = "output" then Some (Printf.sprintf "%s = %s" name value) else None
                            | _ -> None
                        ) streams
                    in
                    Some (String.concat "; " lines)
                  | _ -> None)
              | _ -> None
          ) ce_list
      in
      String.concat "\n" filtered_blocks
    | _ -> "?"

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
                      let counterexample =
                        match List.assoc_opt "counterExample" fields with
                          | Some json -> Some (format_counterexample json)
                          | _ -> None
                      in
                      Some (line, valid, counterexample)
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
