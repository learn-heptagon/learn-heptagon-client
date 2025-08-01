let load_conf () =
  Compiler_options.do_scalarize := true;
  ()

let program p =
  let p_javascript = Obc2javascript.program p in
  let dir = Compiler_utils.build_path "javascript" in
  Compiler_utils.ensure_dir dir;
  Javascript_printer.output_program dir p_javascript
