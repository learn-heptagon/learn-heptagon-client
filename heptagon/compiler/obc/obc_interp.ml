open Idents
open Names
open Types
open Obc

let find_const cenv constname =
  NamesEnv.find constname cenv

let find_class p clsname =
  let rec aux l =
    match l with
    | [] -> failwith (Printf.sprintf "Class %s not found in program" clsname)
    | (Pclass cd)::_ when cd.cd_name.name = clsname -> cd
    | _::l -> aux l
  in aux p.p_desc

let find_method cls metname =
  List.find (fun met -> met.m_name = metname) cls.cd_methods

type value =
  | Vbool of bool
  | Vint of int
  | Vfloat of float
  | Vconstructor of constructor_name
  | Varray of value array
  | Vundef

type memory = {
    mems : value Env.t;
    objs : (obj_dec * memories) Env.t;
  }

and memories =
  | Mem of memory
  | MemA of memories array

let rec get_memory mem idx =
  match mem, idx with
  | Mem mem, [] -> mem
  | MemA arr, hd::tl -> get_memory arr.(hd) tl
  | _ -> invalid_arg "get_memory"

let rec set_memory mem idx m =
  match mem, idx with
  | Mem _, [] -> Mem m
  | MemA arr, hd::tl ->
     let arr' = Array.copy arr in
     arr'.(hd) <- set_memory arr.(hd) tl m;
     MemA arr'
  | _ -> invalid_arg "get_memory"

let init_memory = { mems = Env.empty; objs = Env.empty }

let rec init_memories sizes m =
  match sizes with
  | [] -> Mem m
  | hd::tl ->
     let mems = init_memories tl m in
     MemA (Array.make hd mems)

let env_of_inputs ins args =
  let rec aux ins args env =
    match ins, args with
    | [], [] -> env
    | x::ins, v::args -> aux ins args (Env.add x v env)
    | _, _ -> invalid_arg "env_of_inputs"
  in aux ins args Env.empty

let outputs_of_env env outs =
  List.map (fun x -> try Env.find x env with _ -> Vundef) outs

let fold_eval f env mem l =
  List.fold_left (fun (env, mem) x -> f env mem x) (env, mem) l

let eval_unop op args =
  match args with
  | [v] -> op v
  | _ -> invalid_arg "eval_unop (arity)"

let eval_int_unop op args =
  eval_unop (function Vint v1 -> Vint (op v1) | _ -> Vundef) args

let eval_float_unop op args =
  eval_unop (function Vfloat v1 -> Vfloat (op v1) | _ -> Vundef) args

let eval_bool_unop op args =
  eval_unop (function Vbool v1 -> Vbool (op v1) | _ -> Vundef) args

let eval_int_binop op args =
  match args with
  | [Vint v1; Vint v2] -> Vint (op v1 v2)
  | [_; _] -> Vundef
  | _ -> invalid_arg "eval_int_binop (arity)"

let eval_float_binop op args =
  match args with
  | [Vfloat v1; Vfloat v2] -> Vfloat (op v1 v2)
  | [_; _] -> Vundef
  | _ -> invalid_arg "eval_float_binop (arity)"

let eval_int_comp_binop op args =
  match args with
  | [Vint v1; Vint v2] -> Vbool (op v1 v2)
  | [_; _] -> Vundef
  | _ -> invalid_arg "eval_int_comp_binop (arity)"

let eval_float_comp_binop op args =
  match args with
  | [Vfloat v1; Vfloat v2] -> Vbool (op v1 v2)
  | [_; _] -> Vundef
  | _ -> invalid_arg "eval_float_comp_binop (arity)"

let eval_bool_binop op args =
  match args with
  | [Vbool v1; Vbool v2] -> Vbool (op v1 v2)
  | [_; _] -> Vundef
  | _ -> invalid_arg "eval_bool_binop (arity)"

let eval_op op args =
  match op.qual with
  | Pervasives -> (
    match op.name with
    | "&" -> eval_bool_binop (&&) args
    | "*" -> eval_int_binop (fun x y -> x * y) args
    | "*." -> eval_float_binop (fun x y -> x *. y) args
    | "%" -> eval_int_binop (mod) args
    | "+" -> eval_int_binop (+) args
    | "+." -> eval_float_binop (+.) args
    | "-" -> eval_int_binop (-) args
    | "-." -> eval_float_binop (-.) args
    | "/" -> eval_int_binop (/) args
    | "/." -> eval_float_binop (/.) args
    | "=" -> eval_int_comp_binop (=) args
    | "=." -> eval_float_comp_binop (=) args
    | "<=" -> eval_int_comp_binop (<=) args
    | "<=." -> eval_float_comp_binop (<=) args
    | "<" -> eval_int_comp_binop (<) args
    | "<." -> eval_float_comp_binop (<) args
    | ">=" -> eval_int_comp_binop (>=) args
    | ">=." -> eval_float_comp_binop (>=) args
    | ">" -> eval_int_comp_binop (>) args
    | ">." -> eval_float_comp_binop (>) args
    | "not" -> eval_bool_unop (not) args
    | "or" -> eval_bool_binop (||) args
    | "xor" -> eval_bool_binop (<>) args
    | "~-" -> eval_int_unop (~-) args
    | "~~" -> eval_int_unop (lnot) args
    | ">>>" -> eval_int_binop (lsl) args
    | "<<<" -> eval_int_binop (lsr) args
    | "&&&" -> eval_int_binop (land) args
    | "|||" -> eval_int_binop (lor) args
    | "~-." -> eval_float_unop (~-.) args
    (* TODO do_stuff, between *)
    | "=>" -> eval_bool_binop (fun a b -> (not a) || b) args
    (* TODO exit, assert? *)
    | _ -> failwith "TODO eval_op (pervasives)"
  )
  | LocalModule -> failwith "TODO eval_op (local module)"
  | Module _ | QualModule _ -> failwith "TODO eval_op (module)"

let rec powers v pows =
  match pows with
  | [] -> v
  | (Vint pow)::pows ->
     powers (Varray (Array.make pow v)) pows
  | _::_ -> invalid_arg "powers"

let rec eval_static_exp cenv e =
  match e.se_desc with
  | Svar c -> eval_const cenv c
  | Sint i -> Vint i
  | Sfloat f -> Vfloat f
  | Sbool b -> Vbool b
  | Sconstructor c -> Vconstructor c
  | Sarray_power (e, pows) -> powers (eval_static_exp cenv e) (List.map (eval_static_exp cenv) pows)
  | Sarray es -> Varray (Array.of_list (List.map (eval_static_exp cenv) es))
  | Sop (op, es) -> eval_op op (List.map (eval_static_exp cenv) es)
  | _ -> failwith "TODO eval_static_exp"

and eval_const cenv c =
  (* match c.qual with *)
  (* | LocalModule -> *)
  find_const cenv c.name
  (* | _ -> failwith "TODO eval_const" *)

let rec eval_ext_value cenv env mem e =
  match e.w_desc with
  | Wconst c -> eval_static_exp cenv c
  | Wvar x -> (try Env.find x env with Not_found -> Vundef)
  | Wmem x -> (try Env.find x mem.mems with Not_found -> Vundef)
  | Warray (a, i) ->
     (match eval_ext_value cenv env mem a, eval_exp cenv env mem i with
      | Varray a, Vint i -> a.(i)
      | _, _ -> invalid_arg "eval_ext_value (Warray)")
  | _ -> failwith "TODO eval_ext_value"

and eval_exp cenv env mem e =
  match e.e_desc with
  | Eextvalue e -> eval_ext_value cenv env mem e
  | Eop (op, args) ->
     eval_op op (List.map (eval_exp cenv env mem) args)
  | Estruct _ -> failwith "TODO eval_exp"
  | Earray es ->
     let vs = List.map (eval_exp cenv env mem) es in
     Varray (Array.of_list vs)

let rec eval_pat cenv env mem pat =
  match pat.pat_desc with
  | Lvar x -> (try Env.find x env with Not_found -> Vundef)
  | Lmem x -> (try Env.find x mem.mems with Not_found -> Vundef)
  | Lfield _ -> failwith "TODO eval_pat"
  | Larray (pat, i) ->
     (match eval_pat cenv env mem pat, eval_exp cenv env mem i with
      | Varray a, Vint i -> a.(i)
      | _, _ -> invalid_arg "eval_pat (array)")

let rec assign_pat cenv env mem pat v =
  match pat.pat_desc with
  | Lvar x -> (Env.add x v env, mem)
  | Lmem x -> (env, { mem with mems = Env.add x v mem.mems })
  | Lfield _ -> failwith "TODO assign_pat"
  | Larray (pat, i) ->
     (match eval_pat cenv env mem pat, eval_exp cenv env mem i with
      | Varray a, Vint i ->
         let a' = Array.copy a in
         a'.(i) <- v;
         assign_pat cenv env mem pat (Varray a')
      | _ -> invalid_arg "assign_pat (Larray)")

let rec init_of_type cenv = function
  | Tarray (ty, i) ->
     let v = init_of_type cenv ty in
     (match eval_static_exp cenv i with
      | Vint i -> Varray (Array.make i v)
      | _ -> invalid_arg "init_of_type (Tarray)")
  | _ -> Vundef

let rec env_of_vars cenv vars env =
  match vars with
  | [] -> env
  | vd::vars -> env_of_vars cenv vars (Env.add vd.v_ident (init_of_type cenv vd.v_type) env)

let cenv_of_program p =
  List.fold_left
    (fun cenv -> function
      | Pconst c -> NamesEnv.add c.c_name.name (eval_static_exp cenv c.c_value) cenv
      | _ -> cenv) NamesEnv.empty p.p_desc

let rec eval_act (p: Obc.program) cenv env mem = function
  | Aassgn (pat, e) ->
     assign_pat cenv env mem pat (eval_exp cenv env mem e)
  | Aop (_, _) -> failwith "TODO eval_act Aop"
  | Acall (pats, obj, met, args) ->
     let vs = List.map (eval_exp cenv env mem) args in
     let (vs, mem) = eval_call p cenv env mem obj met vs in
     List.fold_left2 (fun (env, mem) -> assign_pat cenv env mem) (env, mem) pats vs
  | Acase (e, branches) ->
     let cname =
       (match eval_exp cenv env mem e with
        | Vbool b -> if b then "true" else "false"
        | Vconstructor c -> c.name
        | _ -> invalid_arg "eval_act (Acase)") in
     (match List.find_opt (fun (c, _) -> c.name = cname) branches with
      | Some (_, blk) -> eval_block p cenv env mem blk
      | _ -> (env, mem))
  | Afor (var, start, stop, blk) ->
     (match eval_exp cenv env mem start, eval_exp cenv env mem stop with
      | Vint start, Vint stop ->
         let rec loop env mem idx =
           if idx = stop then (env, mem)
           else
             let env = Env.add var.v_ident (Vint idx) env in
             let (env, mem) = eval_block p cenv env mem blk in
             loop env mem (idx + 1)
         in loop env mem start
      | _, _ -> invalid_arg "eval_act (Afor)")
  | Ablock blk -> eval_block p cenv env mem blk

and eval_block p cenv env mem blk =
  let env = env_of_vars cenv blk.b_locals env in
  fold_eval (eval_act p cenv) env mem blk.b_body

and eval_call p cenv env mem obj met args =
  let objid = match obj with Oobj obj | Oarray (obj, _) -> obj in
  let (od, omems) =
    try Env.find objid mem.objs
    with Not_found -> failwith (Printf.sprintf "Obj %s not found in class" (name objid))
  in
  let params = List.map (eval_static_exp cenv) od.o_params in
  let indexes =
    match obj with
    | Oobj obj -> []
    | Oarray (_, pats) ->
       List.map (fun e -> match eval_pat cenv env mem e with
                          | Vint i -> i
                          | _ -> invalid_arg "eval_call (Oarray)") pats
  in
  let omem = get_memory omems indexes in

  let (vs, omem) = call_method p od.o_class met params args omem in
  (vs, { mem with objs = Env.add objid (od, (set_memory omems indexes omem)) mem.objs })

and call_method p clsname metname params args mem =
  match clsname.qual with
  | Module "Mathlib" ->
     let res =
       (match clsname.name with
        | "float" ->
           [eval_unop (function Vint i -> Vfloat (float_of_int i) | _ -> invalid_arg "float (typing)") args]
        | "round" ->
           [eval_unop (function Vfloat f -> Vint (int_of_float f) | _ -> invalid_arg "round (typing)") args]
        | "ceil" -> [eval_float_unop Float.ceil args]
        | "floor" -> [eval_float_unop Float.floor args]
        | "sin" -> [eval_float_unop sin args]
        | "cos" -> [eval_float_unop cos args]
        | "tan" -> [eval_float_unop tan args]
        | "asin" -> [eval_float_unop asin args]
        | "acos" -> [eval_float_unop acos args]
        | "atan" -> [eval_float_unop atan args]
        | "min_float" -> [eval_float_binop Float.min args]
        | "max_float" -> [eval_float_binop Float.max args]
        | "power" -> [eval_float_binop Float.pow args]
        | name -> failwith (Printf.sprintf "TODO call_method (Mathlib.%s)" name)
       )
     in res, mem
  | _ ->
     let cls = find_class p clsname.name in
     let met = find_method cls metname in
     let pars = List.map (fun p -> Signature.(p.p_name)) cls.cd_params in
     let cenv = List.fold_left2 (fun cenv n v -> NamesEnv.add n v cenv) (cenv_of_program p) pars params in
     let ins = List.map (fun vd -> vd.v_ident) met.m_inputs
     and outs = List.map (fun vd -> vd.v_ident) met.m_outputs in
     let env = env_of_vars cenv met.m_outputs (env_of_inputs ins args) in
     let (env, mem) = eval_block p cenv env mem met.m_body in
     outputs_of_env env outs, mem

let rec init_objs p cenv params clsname =
  try
    let cls = find_class p clsname.name in
    let pars = List.map (fun p -> Signature.(p.p_name)) cls.cd_params in
    let cenv = List.fold_left2 (fun cenv n v -> print_endline n; NamesEnv.add n v cenv) cenv pars params in
    let objs = List.fold_left
                 (fun objs od ->
                   let params = List.map (eval_static_exp cenv) od.o_params in
                   let mem = init_objs p cenv params od.o_class in
                   let sizes = Option.fold ~none:[] ~some:(fun x -> x) od.o_size in
                   let sizes = List.map (eval_static_exp cenv) sizes in
                   let mems = init_memories (List.map (function Vint i -> i | _ -> invalid_arg "init_objs") sizes) mem in
                   Env.add od.o_ident (od, mems) objs)
                 Env.empty cls.cd_objs in
    { mems = Env.empty; objs }
  with _ -> init_memory

let reset p clsname =
  let clsname = { qual = LocalModule; name = clsname } in
  let mem = init_objs p (cenv_of_program p) [] clsname in
  try (* In case there is no reset method *)
    snd (call_method p clsname Mreset [] [] mem)
  with Not_found -> mem

let step p clsname args mem =
  call_method p { qual = LocalModule; name = clsname } Mstep [] args mem
