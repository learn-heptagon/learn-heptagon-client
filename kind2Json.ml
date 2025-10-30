(* Json utils *)

open Yojson

exception Malformed_response of string

let get_field f (l: (string * Safe.t) list) =
  print_endline f;
  match List.assoc_opt f l with
  | Some v -> v
  | _ -> raise (Malformed_response (Printf.sprintf "get_field %s" f))

let get_string = function `String s -> s | _ -> raise (Malformed_response "get_string")
let get_string_field f l = get_string (get_field f l)

let get_assoc = function `Assoc a -> a | _ -> raise (Malformed_response "get_assoc")
let get_assoc_field f l = get_assoc (get_field f l)

let get_list = function `List items -> items | _ -> raise (Malformed_response "get_list")
let get_list_field f l = get_list (get_field f l)

let get_int = function `Int i -> i | j -> raise (Malformed_response (Printf.sprintf "get_int (%s)" (Yojson.Safe.to_string j)))
let get_int_field f l = get_int (get_field f l)

let get_int_as_string = function
    | `Int i -> string_of_int i
    | `Intlit s -> s
    | j -> raise (Malformed_response (Printf.sprintf "get_int_as_string (%s)" (Yojson.Safe.to_string j)))
let get_int_field_as_string f l = get_int_as_string (get_field f l)

(** Get "property" field from a Kind2 response *)
let get_property (l: Yojson.Safe.t list) =
  match List.find_opt
          (function
           | `Assoc fields when List.assoc_opt "objectType" fields = Some (`String "property") -> true
           | _ -> false) l
  with
  | Some (`Assoc fields) -> fields
  | _ -> raise (Malformed_response "get_property")
