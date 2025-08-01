open Format
open Names
open Obc
open Obc_utils
open Javascript

let make_var_ident s = Idents.gen_var "obc2javascript" s

let var_idents_of_var_decs (v : Obc.var_dec list) : Javascript.var_ident list =
  List.map (fun v -> v.v_ident) v

let this_field_ident id = Efield (Ethis, Idents.name id)

let fresh_nfor s_l body =
  let id = make_var_ident "id" in
  let rec aux s_l id_l =
    match s_l with
      | [s] -> Javascript.Afor (id, Sint 0, s, Javascript.mk_block (body (List.rev (id::id_l))))
      | s::s_l -> Javascript.Afor (id, Sint 0, s, Javascript.mk_block ([aux s_l (id::id_l)]))
      | [] -> Misc.internal_error "fresh_nfor called with empty size list"
  in
  aux s_l []

let translate_const_name { qual = m; name = n } =
  { qual = QualModule { qual = m; name = "constantes"};
    name = String.uppercase_ascii n }

let qualname_to_class_name q =
  { qual = q.qual; name = String.capitalize_ascii q.name }

let qualname_to_package_classe q =
  { qual = q.qual; name = String.capitalize_ascii q.name }

let find_index p l =
  Option.map fst (List.find_opt (fun (_, x) -> p x) (List.mapi (fun i x -> (i, x)) l))

let translate_constructor_name q =
  let tyid = Modules.find_constrs q in
  let ty = Modules.find_type tyid in
  match ty with
  | Signature.Tenum cs -> Option.get (find_index (fun c -> c = q) cs)
  | _ -> invalid_arg "translate_constructor_name"

let translate_field_name f = f |> Names.shortname |> String.lowercase_ascii

let rec static_exp param_env se =
  match se.Types.se_desc with
    | Types.Svar c ->
      (match c.qual with
        | LocalModule ->
          let n = NamesEnv.find (shortname c) param_env in
          Svar (n |> Idents.name |> local_qn)
        | _ -> Svar (translate_const_name c))
    | Types.Sint i -> Sint i
    | Types.Sfloat f -> Sfloat f
    | Types.Sbool b -> Sbool b
    | Types.Sstring s -> Sstring s
    | Types.Sconstructor c -> let c = translate_constructor_name c in Sint c
    | Types.Sfield _ -> eprintf "ojSfield @."; assert false;
    | Types.Stuple se_l ->  tuple param_env se_l
    | Types.Sarray_power (see,pow_list) ->
      let pow_list = List.rev pow_list in
      let rec make_array vi pow_list =
        match pow_list with
          | pow::pow_list ->
            let pow = (try Static.int_of_static_exp Names.QualEnv.empty pow
                       with  Errors.Error ->
                                   eprintf "%aStatic power of array should have integer power. \
                                           Please use callgraph or non-static exp in %a.@."
                              Location.print_location se.Types.se_loc
                              Global_printer.print_static_exp se;
                              raise Errors.Error)
            in
            Javascript.Earray (Misc.repeat_list (make_array vi pow_list) pow)
          | _ -> static_exp param_env see
      in
      make_array (make_var_ident "id") pow_list
    | Types.Sarray se_l ->
      Javascript.Earray (List.map (static_exp param_env) se_l)
    | Types.Srecord f_e_l ->
      let ty_name =
        match se.Types.se_ty with
        | Types.Tid ty_name -> qualname_to_package_classe ty_name
        | _ -> Misc.internal_error "Obc2javascript"
      in
      let f_e_l =
        List.sort
          (fun (f1,_) (f2,_) -> compare f1.name f2.name)
          f_e_l in
      let e_l = List.map (fun (_f,e) -> e) f_e_l in
      Enew (ty_name, List.map (static_exp param_env) e_l)
    | Types.Sop (f, se_l) -> Efun (f, List.map (static_exp param_env) se_l)

and exp param_env e =
  match e.e_desc with
    | Obc.Eextvalue p -> ext_value param_env p
    | Obc.Eop (op, e_l) -> Efun (op, exp_list param_env e_l)
    | Obc.Estruct (ty_name, f_e_l) ->
      let ty_name = qualname_to_package_classe ty_name in
      let f_e_l =
        List.sort
          (fun (f1,_) (f2,_) -> compare f1.name f2.name)
          f_e_l in
      let e_l = List.map (fun (_f,e) -> e) f_e_l in
      Enew (ty_name, exp_list param_env e_l)
    | Obc.Earray e_l -> Javascript.Earray (exp_list param_env e_l)

and exp_list param_env e_l = List.map (exp param_env) e_l

and tuple param_env se_l =
  Javascript.Earray (List.map (static_exp param_env) se_l)

and pattern param_env p =
  match p.pat_desc with
    | Obc.Lvar v -> Pvar v
    | Obc.Lmem v -> Pthis v
    | Obc.Lfield (p,f) -> Pfield (pattern param_env p, translate_field_name f)
    | Obc.Larray _ ->
      let p, idx_l =
        let rec gather_idx acc p =
          match p.pat_desc with
            | Obc.Larray (p,e) -> gather_idx ((exp param_env e)::acc) p
            | _ -> pattern param_env p, acc
        in
        let p, idx_l = gather_idx [] p in
        p, idx_l
      in
      Parray_elem (p, idx_l)

and pattern_to_exp param_env p =
  match p.pat_desc with
    | Obc.Lvar v -> Evar v
    | Obc.Lmem v -> this_field_ident v
    | Obc.Lfield (p,f) ->
      Efield (pattern_to_exp param_env p, translate_field_name f)
    | Obc.Larray _ ->
      let p, idx_l =
        let rec gather_idx acc p =
          match p.pat_desc with
            | Obc.Larray (p,e) -> gather_idx ((exp param_env e)::acc) p
            | _ -> pattern_to_exp param_env p, acc
        in
        let p, idx_l = gather_idx [] p in
        p, idx_l
      in
      Earray_elem (p, idx_l)

and ext_value param_env w =
  match w.w_desc with
    | Obc.Wvar v -> Evar v
    | Obc.Wconst c -> static_exp param_env c
    | Obc.Wmem v -> this_field_ident v
    | Obc.Wfield (p,f) -> Efield (ext_value param_env p, translate_field_name f)
    | Obc.Warray (p,e) -> Earray_elem (ext_value param_env p, [exp param_env e])

let obj_ref param_env o =
  match o with
    | Oobj id -> this_field_ident id
    | Oarray (id, p_l) ->
      let idx_l = List.map (fun p -> pattern_to_exp param_env p) p_l in
      Earray_elem (this_field_ident id, idx_l)

let jop_of_op param_env op_name e_l =
  match op_name with
    | _ ->
      Efun (op_name, exp_list param_env e_l)

let rec act_list param_env act_l acts =
  let _act act acts =
    match act with
      | Obc.Aassgn (p,e) -> (Javascript.Aassgn (pattern param_env p, exp param_env e))::acts
      | Obc.Aop (op,e_l) -> Aexp (jop_of_op param_env op e_l) :: acts
      | Obc.Acall ([], obj, Mstep, e_l) ->
        let acall = Emethod_call (obj_ref param_env obj, "step", exp_list param_env e_l) in
        Aexp acall::acts
      | Obc.Acall ([p], obj, Mstep, e_l) ->
        let ecall = Emethod_call (obj_ref param_env obj, "step", exp_list param_env e_l) in
        let assgn = Javascript.Aassgn (pattern param_env p, ecall) in
        assgn::acts
      | Obc.Acall (p_l, obj, Mstep, e_l) ->
        let ecall = Emethod_call (obj_ref param_env obj, "step", exp_list param_env e_l) in
        let assgn = Javascript.Aassgn (Parray (List.map (pattern param_env) p_l), ecall) in
        assgn::acts
      | Obc.Acall (_, obj, Mreset, _) ->
        let acall = Emethod_call (obj_ref param_env obj, "reset", []) in
        Aexp acall::acts
      | Obc.Acase (e, c_b_l) when e.e_ty = Types.Tid Initial.pbool ->
        (match c_b_l with
          | [] -> acts
          | [(c,b)] when c = Initial.ptrue ->
              (Aif (exp param_env e, block param_env b)):: acts
          | [(c,b)] when c = Initial.pfalse ->
              (Aifelse (exp param_env e, {Javascript.b_locals = [];
                                          Javascript.b_body = []},
                        block param_env b)) :: acts
          | _ ->
              let _, _then = List.find (fun (c,_) -> c = Initial.ptrue) c_b_l in
              let _, _else = List.find (fun (c,_) -> c = Initial.pfalse) c_b_l in
              (Aifelse (exp param_env e, block param_env _then, block param_env _else)) :: acts)
      | Obc.Acase (e, c_b_l) ->
        let _c_b (c,b) =
          Sint (translate_constructor_name c),
          block param_env b in
        let acase = Aswitch (exp param_env e, List.map _c_b c_b_l) in
        acase::acts
      | Obc.Afor (v, se, se', b) ->
        let afor = Javascript.Afor (v.v_ident,
                              exp param_env se, exp param_env se',
                              block param_env b) in
        afor::acts
      | Obc.Ablock b ->
        let ablock = Javascript.Ablock (block param_env b) in
        ablock::acts
  in
  List.fold_right _act act_l acts

and block param_env ?(locals=[]) ?(end_acts=[]) ob =
  let blocals = var_idents_of_var_decs ob.Obc.b_locals in
  let locals = locals @ blocals in
  let acts = act_list param_env ob.Obc.b_body end_acts in
  { Javascript.b_locals = locals; Javascript.b_body = acts }

let sig_params_to_vis p_l =
  let param_to_arg param_env p =
    let p_ident = Idents.gen_var "obc2javascript" (String.uppercase_ascii p.Signature.p_name) in
    let param_env = NamesEnv.add p.Signature.p_name p_ident param_env in
    p_ident, param_env
  in Misc.mapfold param_to_arg NamesEnv.empty p_l

let copy_to_this vi_l =
  let _vi vi = Javascript.Aassgn (Pthis vi, Evar vi) in
  List.map _vi vi_l

let class_def_list class_descs cd_l =
  let class_def class_descs cd =
    Idents.enter_node cd.Obc.cd_name;
    let class_name = qualname_to_package_classe cd.Obc.cd_name in
    let fields_params, vis_params, _exps_params, param_env =
      let v, env = sig_params_to_vis cd.cd_params in
      let f = vis_to_fields v in
      let e = vis_to_exps v in
      f, v, e, env
    in

    let reset, reset_mems =
      try
        let oreset = find_reset_method cd in
        let body = block param_env oreset.Obc.m_body in
        let reset_mems = block param_env (remove_resets oreset.Obc.m_body) in
        mk_methode body "reset", reset_mems
      with Not_found ->
        mk_methode (Javascript.mk_block []) "reset", Javascript.mk_block []
    in

    let constructeur =
      let body =
        let obj_init_act acts od =
          let obj_id = od.o_ident in
          let class_id = (qualname_to_class_name od.o_class) in
          let params = List.map (static_exp param_env) od.o_params in
          match od.o_size with
            | None ->
                (Javascript.Aassgn (Pthis obj_id, Enew (class_id, params)))::acts
            | Some size_l ->
                let size_l = List.rev (List.map (static_exp param_env) size_l) in
                let assgn_elem i_l =
                  [ Javascript.Aassgn (Parray_elem (Pthis obj_id, List.map mk_var i_l),
                                 Enew (class_id, params)) ]
                in
                (Javascript.Aassgn (Pthis od.o_ident, Javascript.Earray []))
                 :: (fresh_nfor size_l assgn_elem)
                 :: acts
        in

        let acts = [Javascript.Ablock reset_mems] in
        let acts = List.fold_left obj_init_act acts cd.cd_objs in
        let acts = (copy_to_this vis_params)@acts in
        { Javascript.b_locals = []; Javascript.b_body = acts }
      in
      mk_methode ~args:vis_params body (shortname class_name)
    in

    let fields =
      let mem_to_field fields v =
        (Javascript.mk_field v.v_ident) :: fields
      in
      let obj_to_field fields od = (Javascript.mk_field od.o_ident) :: fields in
      let fields = fields_params in
      let fields = List.fold_left mem_to_field fields cd.cd_mems in
      List.fold_left obj_to_field fields cd.cd_objs
    in
    let step =
      let ostep = find_step_method cd in
      let v_output = ostep.m_outputs in
      let return_act =
        Areturn (match v_output with
                  | [] -> Evoid
                  | [v] -> Evar v.v_ident
                  | v_l -> Javascript.Earray (List.map (fun v -> Evar v.v_ident) v_l)
                )
      in
      let body = block param_env ~locals:(var_idents_of_var_decs v_output) ~end_acts:[return_act] ostep.Obc.m_body in
      mk_methode ~args:(var_idents_of_var_decs ostep.Obc.m_inputs) body "step"
    in
    let class_desc = mk_class_desc ~fields:fields ~constrs:[constructeur] ~methodes:[step;reset] class_name in
    class_desc::class_descs
  in
  List.fold_left class_def class_descs cd_l

(* TODO *)

let program p =
  let rec program_descs pds (ns,cs,ts) =
    match pds with
      | [] -> ns,cs,ts
      | Obc.Pclass n :: pds -> program_descs pds (n::ns,cs,ts)
      | Obc.Pconst c :: pds -> program_descs pds (ns,c::cs,ts)
      | Obc.Ptype t :: pds -> program_descs pds (ns,cs,t::ts)
  in
  let ns,_cs,_ts = program_descs p.p_desc ([],[],[]) in
  class_def_list [] ns
