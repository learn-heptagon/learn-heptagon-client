open Notebook

let editor1_content = "\
node max(fst, snd : real) returns (out : real)
let
    (* TODO *)
tel
"

let editor2_content = "\
-- node abs(x : real) returns (out : real)
-- let
--     (* TODO *)
-- tel
"

let editor3_content = "\
-- node saturate(l_bound, x, u_bound : real) returns (out : real)
-- let
--     (* TODO *)
-- tel
"

let editor4_content = "\
node full_adder(a, b, c: bool) returns (s, co : bool)
let
    (* TODO *)
tel
"

let editor5_content = "\
node half_adder(a, b: bool) returns (s, c: bool)
let
    (* TODO *)
tel
"

let editor6_content = "\
node full_adder_h(a, b, c: bool) returns (s, co : bool)
let
    (* TODO *)
tel
"

let notebook1 = {
  title = "combinatorial.lus";
  cells = [Text "combinatorial";
           Editor { editor_id = 1; editor_content = editor1_content; };
           Editor { editor_id = 2; editor_content = editor2_content; };
           Editor { editor_id = 3; editor_content = editor3_content; }];
  }

and notebook2 = {
  title = "full_adder.lus";
  cells = [Text "full_adder";
           Editor { editor_id = 4; editor_content = editor4_content; };
           Editor { editor_id = 5; editor_content = editor5_content; };
           Editor { editor_id = 6; editor_content = editor6_content; }];
  }

let my_notebooks = [notebook1; notebook2]
