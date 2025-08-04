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
-- node saturate(lbound, x, ubound : real) returns (out : real)
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

(* TODO ppx ? *)

let notebook1 = {
  title = "combinatorial.lus";
  cells = [
      Heading "Combinatorial Programs";
      Text "In this first exercise, we will implement a few combinatorial nodes,
            that is, nodes which outputs depend only on the current values of their inputs.";
      Html "Write a node <code>max(fst, snd: real) returns (out: real)</code>,
            where <code>out</code> is the maximum between <code>fst</code> and <code>snd</code>.
            To compare reals, use the <code><.</code> operator.";
      Editor { editor_id = 1; editor_content = editor1_content; };
      Html "Write a node <code>abs(x: real) returns (out: real)</code>,
            which computes the absolute value of its input.
            Remember that you need to use operators on real numbers (<code>+.</code>, <code>-.</code>, ...).";
      Editor { editor_id = 2; editor_content = editor2_content; };
      Html "Write a node <code>saturate(lbound, x, ubound: real) returns (out: real)</code>,
            which returns <code>x</code> if it between <code>lbound</code> and <code>ubound</code>,
            or one of the bounds if <code>x</code> is too small or too big.";
      Editor { editor_id = 3; editor_content = editor3_content; }];
  }

and notebook2 = {
  title = "full_adder.lus";
  cells = [Heading "full_adder";
           Editor { editor_id = 4; editor_content = editor4_content; };
           Editor { editor_id = 5; editor_content = editor5_content; };
           Editor { editor_id = 6; editor_content = editor6_content; }];
  }

let my_notebooks = [notebook1; notebook2]
