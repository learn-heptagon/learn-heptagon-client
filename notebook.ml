type editor_data = {
  editor_id : int;
  mutable editor_content : string;
}

type cell =
  | Heading of string
  | Text of string
  | Html of string
  | Editor of editor_data

type notebook = {
  title : string;
  cells : cell list;
}
