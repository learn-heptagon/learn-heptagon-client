open Js_of_ocaml
open Js_of_ocaml_lwt

let (let*) = Lwt.bind


(** Client part to perform the "/autocorrect" request **)

(** URL that the programs to auto-correct should be sent to *)
let correct_url =
  let url = Printf.sprintf "%s/autocorrect" Verify.current_url in
  Option.get (Url.url_of_string url)

(** Send an auto-correct request for [prog] to the kind2 server, and handle the result *)
let send_correct filename ndname ins outs prog =
  let content =
    Yojson.Safe.to_string
      (`Assoc [("file_name", `String filename);
               ("node_name", `String ndname);
               ("ins", `List (List.map (fun s -> `String s) ins));
               ("outs", `List (List.map (fun s -> `String s) outs));
               ("prog", `String prog)])
  in
  XmlHttpRequest.perform
    ~content_type:"application/json"
    ~contents:(`String content) correct_url

let get_sig (prog: Minils.program) =
  let open Minils in
  let nodes = List.filter_map (function Pnode n -> Some n | _ -> None) prog.p_desc in
  match List.rev nodes with
  | nd::_ ->
     let typs = List.map
                  (fun vd ->
                    Kind2_printer.print_type Format.str_formatter vd.v_type;
                    Idents.source_name vd.v_ident, Format.flush_str_formatter ()) in
     Names.shortname nd.n_name, typs nd.n_input, typs nd.n_output
  | _ -> invalid_arg "get_ins_outs"

type counterexample = (string * (string * string list) list) list

let format_counterexamples ins outs (json : Yojson.Safe.t) : counterexample =
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
        if cls = "input" then
          let i = int_of_string (String.sub name 2 (String.length name - 2)) in
          Some (fst (List.nth ins i), values)
        else if cls = "local" then
          let i = int_of_string (String.sub name 3 (String.length name - 3)) in
          let exp = if String.sub name 1 1 = "2" then "expected" else "yours" in
          Some (Printf.sprintf "%s (%s)" (fst (List.nth outs i)) exp, values)
        else None
      ) streams
    in
    Some (block_name, ce_list)
  ) ce_list

type result = Valid | Falsifiable of counterexample | Unknown | Error of string

(** Entrypoint for autocorrect *)
let autocorrect title prog =
  (* Send the request *)
  let (name, ins, outs) = get_sig prog in
  let* res =
    Kind2_printer.print_program ~with_contract:false Format.str_formatter prog;
    send_correct title name
      (List.map snd ins) (List.map snd outs)
      (Format.flush_str_formatter ())
  in
  (* Handle the response *)
  let open Kind2Json in
  match res.code with
  | 200 ->
     (try
        let json = Yojson.Safe.from_string res.content in
        let props = get_property (get_list json) in
        let res = get_string_field "value" (get_assoc_field "answer" props) in
        match res with
        | "valid" -> Lwt.return Valid
        | "falsifiable" ->
           let counterexample = get_field "counterExample" props in
           Lwt.return (Falsifiable (format_counterexamples ins outs counterexample))
        | _ -> Lwt.return Unknown
      with
      | Yojson.Json_error msg ->
         Lwt.return (Error (Printf.sprintf "JSON parsing error: %s" msg))
      | Malformed_response ->
         Lwt.return (Error "Response is malformed"))
  | _ ->
     Lwt.return (Error (Printf.sprintf "%d: %s" res.code res.content))
