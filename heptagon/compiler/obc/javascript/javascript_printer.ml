open Javascript
open Pp_tools
open Format

let print_ident ff id =  Format.fprintf ff "%s" (jsname_of_name (Idents.name id))

let print_qualname ff ({ Names.name = n } as qn) =
  Global_printer.print_qualname ff { qn with Names.name = jsname_of_name n }

let javascript_print_string ff s =
  pp_print_string ff (jsname_of_name s)

let class_name = print_qualname
let const_name = print_qualname
let field_name = javascript_print_string
let method_name = javascript_print_string

let print_field_ident = print_ident
let print_var_ident = print_ident

let static ff s = if s then fprintf ff "static " else ()

let var_ident init ff vi =
  if init then
    fprintf ff "let %a = null" print_var_ident vi
  else
    fprintf ff "let %a" print_var_ident vi

let vi_list ff vi_l = match vi_l with
  | [] -> ()
  | _ -> fprintf ff "%a\n" (print_list_r (var_ident true) "" ";" ";") vi_l

let rec exp ff = function
  | Ethis -> fprintf ff "this"
  | Efun (f, e_l) -> op ff (f, e_l)
  | Emethod_call (o, m, e_l) -> fprintf ff "%a.%a%a" exp o method_name m args e_l
  | Enew (cn, e_l) -> fprintf ff "new %a%a" print_qualname cn args e_l
  | Evoid -> ()
  | Svar c -> const_name ff c
  | Sint i -> pp_print_int ff i
  | Sfloat f -> fprintf ff "%g" f
  | Sbool b -> pp_print_bool ff b
  | Sstring s -> fprintf ff "\"%s\"" (String.escaped s)
  | Snull -> fprintf ff "null"
  | Efield (p, f) -> fprintf ff "%a.%a" exp p field_name f
  | Evar vi -> print_var_ident ff vi
  | Eclass c -> class_name ff c
  | Earray_elem (p, e_l) -> fprintf ff "%a%a" exp p (print_list exp "[" "][" "]") e_l
  | Earray e_l -> fprintf ff "[%a]" (print_list_r exp "" "," "") e_l
and op ff (f, e_l) =
  let javascriptop = function
    | "="  -> "==="
    | "<>" -> "!=="
    | "or" -> "||"
    | "&"  -> "&&"
    | "*." -> "*"
    | "/." -> "/"
    | "+." -> "+"
    | "-." -> "-"
    | "~~" -> "~"
    | "<<<" -> "<<"
    | ">>>" -> ">>"
    | "&&&" -> "&"
    | "|||" -> "|"
    | "<." -> "<"
    | "<=." -> "<="
    | ">." -> ">"
    | ">=." -> ">="
    | op   -> op
  in
  match Names.modul f with
  | Names.Pervasives ->
      (match Names.shortname f with
        |("+" | "-" | "*" | "/" | "%"
        |"+." | "-." | "*." | "/."
        | "=" | "<>" | "<" | "<." | "<="
        | "<=." | ">" | ">." | ">=" | ">=." | "&" | "or") as n ->
           let e1,e2 = Misc.assert_2 e_l in
           fprintf ff "(%a %s %a)" exp e1 (javascriptop n) exp e2
        | "not" ->
            let e = Misc.assert_1 e_l in
            fprintf ff "!%a" exp e
        | "~-" | "~-." ->
            let e = Misc.assert_1 e_l in
            fprintf ff "-%a" exp e
        | "=>" ->
           let e1,e2 = Misc.assert_2 e_l in
           fprintf ff "(!%a || %a)" exp e1 exp e2
        | "assert" ->
            let e = Misc.assert_1 e_l in
            fprintf ff "assert(%a)" exp e
        | _ -> failwith "TO DO")
  | _ -> fprintf ff "%a%a" Global_printer.print_qualname f args e_l

and args ff e_l = fprintf ff "(%a)" (print_list_r exp "" "," "") e_l

let rec pattern ff = function
  | Pfield (p, f) -> fprintf ff "%a.%a" pattern p field_name f
  | Pvar vi -> print_var_ident ff vi
  | Parray_elem (p, e_l) -> fprintf ff "%a%a" pattern p (print_list exp "[" "][" "]") e_l
  | Parray p_l -> fprintf ff "[%a]" (print_list_r pattern "" "," "") p_l
  | Pthis f -> fprintf ff "this.%a" print_field_ident f

let rec block ff b =
  fprintf ff "%a%a"
    (vi_list) b.b_locals
    (print_list_r act "" "" "\n") b.b_body

and act ff = function
  | Anewvar (vi, e) -> fprintf ff "let %a = %a;" (var_ident false) vi exp e
  | Aassgn (p, e) -> fprintf ff "%a = %a;" pattern p exp e
  | Aexp e -> fprintf ff "%a;" exp e
  | Aswitch (e, c_b_l) ->
      let pcb ff (e, b) =
        let print_case ff e = exp ff e in
        fprintf ff "case %a: %a break;" print_case e block b in
      fprintf ff "switch (%a) { %a }"
        exp e
        (print_list_r pcb "" "" "") c_b_l
  | Aif (e, bt) ->
      fprintf ff "if (%a) %a" exp e block bt
  | Aifelse (e, bt, bf) ->
      fprintf ff "if (%a) {%a} else {%a}"
        exp e
        block bt
        block bf
  | Ablock b -> if (List.length b.b_body > 0) then fprintf ff "{ %a }" block b
  | Afor (vi, i1, i2, b) ->
      fprintf ff "for (%a = %a; %a<%a; %a++) %a"
        (var_ident false) vi
        exp i1
        print_var_ident vi
        exp i2
        print_var_ident vi
        block b
  | Areturn e -> fprintf ff "return %a;" exp e

let methode ff m =
  fprintf ff "%a %a(%a) {\n%a\n}\n"
    static m.m_static
    method_name m.m_name
    (print_list_r (fun ff vi -> print_var_ident ff vi) "" "," "") m.m_args
    block m.m_body

let constructor ff m =
  fprintf ff "constructor(%a) {\n%a\n}\n"
    (print_list_r (fun ff vi -> print_var_ident ff vi) "" "," "") m.m_args
    block m.m_body

let class_desc ff cd =
  fprintf ff "class %a {\n%a%a}\n"
    Global_printer.print_qualname cd.cd_name
    (print_list constructor "" "" "") cd.cd_constructors
    (print_list methode "" "" "") cd.cd_methodes

let program ff p  =
  List.iter (fun cd -> class_desc ff cd) p;
  fprintf ff "@."

let output_program base_dir (p:Javascript.program) =
  match p with
    | [] -> ()
    | cd :: _ ->
      let { Names.name = file_name; Names.qual = package } = cd.cd_name in
      let file_name = (jsname_of_name file_name) ^ ".js" in
      let package_dirs = Misc.split_string (Names.modul_to_string package) "." in
      let create_dir base_dir dir =
        let dir = Filename.concat base_dir dir in
        Compiler_utils.ensure_dir dir;
        dir
      in
      let dir = List.fold_left create_dir base_dir package_dirs in
      let oc = open_out (Filename.concat dir file_name) in
      let ff = Format.formatter_of_out_channel oc in
      pp_set_margin ff 120;

      program ff p;

      close_out oc
