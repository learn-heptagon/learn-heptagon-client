type editor_data = {
  editor_id : int;
  editor_title : string;
  editor_depends : string list;
  (* The next fields are modified at ntime *)
  mutable editor_content : string;
  mutable editor_compiles : bool;
  mutable editor_callbacks : (bool -> unit) list;
}

type cell =
  | Heading of string
  | Text of string
  | Html of string
  | Editor of editor_data

type notebook = {
  title : string;
  cells : cell list;
  has_autocorrect : bool;
}
