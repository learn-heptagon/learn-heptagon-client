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

let get_properties_infos prog =
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
                       Some (line, valid)
                    | _ -> None)
                ) items
            | _ -> []
        in

        (* Sort line numbers in ascending order*)
        let sorted_props = List.sort (fun (x, _) (y, _) -> compare x y) props in

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
