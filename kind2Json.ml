(* Json utils *)

open Yojson

exception Malformed_response

let get_field f (l: (string * Safe.t) list) =
  match List.assoc_opt f l with
  | Some v -> v
  | _ -> raise Malformed_response

let get_string = function `String s -> s | _ -> raise Malformed_response
let get_string_field f l = get_string (get_field f l)

let get_assoc = function `Assoc a -> a | _ -> raise Malformed_response
let get_assoc_field f l = get_assoc (get_field f l)

let get_list = function `List items -> items | _ -> raise Malformed_response
let get_list_field f l = get_list (get_field f l)

let get_int = function `Int i -> i | _ -> raise Malformed_response
let get_int_field f l = get_int (get_field f l)

(** Get "property" field from a Kind2 response *)
let get_property (l: Yojson.Safe.t list) =
  match List.find_opt
          (function
           | `Assoc fields when List.assoc_opt "objectType" fields = Some (`String "property") -> true
           | _ -> false) l
  with
  | Some (`Assoc fields) -> fields
  | _ -> raise Malformed_response
