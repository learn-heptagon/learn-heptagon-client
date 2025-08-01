type class_name = Names.qualname

type obj_ident = Idents.var_ident

type const_name = Names.qualname
type method_name = Names.name
type field_name = Names.name
type field_ident = Idents.var_ident
type op_name = Names.qualname
type var_ident = Idents.var_ident

and class_desc = { cd_name         : class_name;
                   cd_fields       : field list;
                   cd_constructors : methode list;
                   cd_methodes     : methode list; }

and field = { f_static : bool;
              f_ident  : field_ident;
              f_value  : exp option }

and methode = { m_static  : bool;
                m_name    : method_name;
                m_args    : var_ident list;
                m_body    : block; }

and block = { b_locals : var_ident list;
              b_body   : act list; }

and act = Anewvar of var_ident * exp
        | Aassgn of pattern * exp
        | Aexp of exp
        | Aswitch of exp * (exp * block) list
        | Aif of exp * block
        | Aifelse of exp * block * block
        | Ablock of block
        | Afor of var_ident * exp * exp * block
        | Areturn of exp

and exp = Ethis
        | Efun of op_name * exp list
        | Emethod_call of exp * method_name * exp list
        | Enew of class_name * exp list
        | Evoid
        | Svar of const_name
        | Sint of int
        | Sfloat of float
        | Sbool of bool
        | Sstring of string
        | Snull
        | Efield of exp * field_name
        | Eclass of class_name
        | Evar of var_ident
        | Earray_elem of exp * exp list
        | Earray of exp list

and pattern = Pfield of pattern * field_name
            | Pvar of var_ident
            | Parray_elem of pattern * exp list
            | Parray of pattern list
            | Pthis of field_ident

type program = class_desc list

let jsname_of_name name =
  let buf = Buffer.create (String.length name) in
  let convert c =
    match c with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '$' ->
          Buffer.add_char buf c
      | '\'' -> Buffer.add_string buf "_prime"
      | _ ->
          Buffer.add_string buf "lex";
          Buffer.add_string buf (string_of_int (Char.code c)) in
  String.iter convert name;
  Buffer.contents buf

let mk_var x = Evar x

let mk_block ?(locals=[]) b =
  { b_locals = locals;
    b_body   = b; }

let mk_methode ?(static=false) ?(args=[]) body name =
  { m_static = static;
    m_name   = name;
    m_args   = args;
    m_body   = body; }

let mk_class_desc ?(fields=[]) ?(constrs=[]) ?(methodes=[]) class_name =
  { cd_name         = class_name;
    cd_fields       = fields;
    cd_constructors = constrs;
    cd_methodes     = methodes; }

let mk_field ?(static = false) ?(value = None) ident =
  { f_static = static;
    f_ident  = ident;
    f_value  = value }

let vis_to_exps vi_l = List.map (fun x -> mk_var x) vi_l
let vis_to_fields vi_l = List.map (fun x -> mk_field x) vi_l
