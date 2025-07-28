let editor11_content = "\
node max(fst, snd : real) returns (out : real)
let
    (* TODO *)
tel
"

let editor12_content = "\
-- node abs(x : real) returns (out : real)
-- let
--     (* TODO *)
-- tel
"

let editor13_content = "\
-- node saturate(l_bound, x, u_bound : real) returns (out : real)
-- let
--     (* TODO *)
-- tel
"

let editor21_content = "\
node full_adder(a, b, c: bool) returns (s, co : bool)
let
    (* TODO *)
tel
"

let editor22_content = "\
node half_adder(a, b: bool) returns (s, c: bool)
let
    (* TODO *)
tel
"

let editor23_content = "\
node full_adder_h(a, b, c: bool) returns (s, co : bool)
let
    (* TODO *)
tel
"

let contents_of_notebook1 = [editor11_content; editor12_content; editor13_content]
let contents_of_notebook2 = [editor21_content; editor22_content; editor23_content]
let txts_of_notebooks = ["combinatorial"; "full_adder"]
