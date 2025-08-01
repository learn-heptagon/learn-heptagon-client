(***********************************************************************)
(*                                                                     *)
(*                             Heptagon                                *)
(*                                                                     *)
(* Gwenael Delaval, LIG/INRIA, UJF                                     *)
(* Leonard Gerard, Parkas, ENS                                         *)
(* Adrien Guatto, Parkas, ENS                                          *)
(* Cedric Pasteur, Parkas, ENS                                         *)
(* Marc Pouzet, Parkas, ENS                                            *)
(*                                                                     *)
(* Copyright 2012 ENS, INRIA, UJF                                      *)
(*                                                                     *)
(* This file is part of the Heptagon compiler.                         *)
(*                                                                     *)
(* Heptagon is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published by   *)
(* the Free Software Foundation, either version 3 of the License, or   *)
(* (at your option) any later version.                                 *)
(*                                                                     *)
(* Heptagon is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(* GNU General Public License for more details.                        *)
(*                                                                     *)
(* You should have received a copy of the GNU General Public License   *)
(* along with Heptagon.  If not, see <http://www.gnu.org/licenses/>    *)
(*                                                                     *)
(***********************************************************************)
(* removing reset statements *)

(* REQUIRES automaton stateful present *)

open Misc
open Heptagon
open Hept_utils
open Types
open Initial

(* This pass performs two tasks:

   1. eliminate reset blocks;

   2. lower the "fby" and "init" (->) operators to possibly-initialized "pre".

   This is done in a single pass over the syntax tree, by propagating
   disjunctions of reinitialization conditions. In the general case:

   - "c fby e2" and "pre(c, e2)" rewrite to "if rst then c else pre(c, e2)";

   - "e1 fby e2" rewrites to "if rst or (true -> false) then e1 else pre e2";

   - "e1 -> e2" rewrites to "if rst or (true -> false) then e1 else e2".

   In the first case, we avoid useless if statements when "rst" is the condition
   that is always false. *)

let fresh = Idents.gen_fresh "reset" ~reset:true (fun () -> "r")

(* get e and return r, var_dec_r, r = e *)
let reset_var_from_exp e =
  let r = fresh() in
  { e with e_desc = Evar r },
  mk_var_dec r (Tid Initial.pbool) ~linearity:Linearity.Ltop,
  mk_equation (Eeq(Evarpat r, e))

(** Merge two reset conditions *)
let merge_resets res1 res2 =
  let mk_or e1 e2 = mk_op_app (Efun Initial.por) [e1;e2] in
  match res1, res2 with
    | None, _ -> res2
    | _, None -> res1
    | Some e1, Some e2 -> Some { e1 with e_desc = mk_or e1 e2 }

(* [if_opt_cond ~whentrue ~otherwise cond] returns the Heptagon expression "if
   cond then whentrue else otherwise" when [cond] is not [None], and [otherwise]
   otherwise. *)
let if_opt_cond ~whentrue ~otherwise = function
  | None -> otherwise.e_desc
  | Some cond -> mk_op_app Eifthenelse [cond; whentrue; otherwise]

(* [if_opt_cond_or_init ~whentrue ~otherwise cond] returns the Heptagon
   expression "if cond or (true fby false) then whentrue else otherwise". *)
let if_opt_cond_or_init ~whentrue ~otherwise cond =
  let init =
    mk_exp (Epre (Some (mk_static_bool true), dfalse))
      ~loc:otherwise.e_loc (Tid Initial.pbool) ~linearity:Linearity.Ltop
  in
  if_opt_cond ~whentrue ~otherwise (merge_resets (Some init) cond)

let edesc funs ((res,_) as acc) ed =
  match ed with
    | Epre (Some c, e) | Efby ({ e_desc = Econst c; }, e) ->
       (* The initialized delay "pre (c, e)", possibly written as a fby, is
          implemented as

            [if rst then c else pre (c, e2)].
        *)
       let e,_ = Hept_mapfold.exp_it funs acc e in
       if_opt_cond
         ~whentrue:(mk_exp (Econst c) (e.e_ty) ~linearity:Linearity.Ltop)
         ~otherwise:{ e with e_desc = Epre (Some c, e); }
         res,
       acc
    | Efby (e1, e2) ->
       (* Since [e1] is not a constant, we must translate [e1 fby e2] to

            [if rst or (true fby false) then e1 else pre e2].

          Omitting the second term in the disjunction is incorrect
          w.r.t. initialization.
        *)
        let e1, _ = Hept_mapfold.exp_it funs acc e1 in
        let e2, _ = Hept_mapfold.exp_it funs acc e2 in
        if_opt_cond_or_init
          ~whentrue:e1
          ~otherwise:{ e2 with e_desc = Epre(None, e2) }
          res,
        acc
    | Eapp({ a_op = Earrow }, [e1; e2], _) ->
       (* We translate [e1 -> e2] to

            [if rst or (true fby false) then e1 else e2].

          Omitting the second term in the disjunction is incorrect since "rst"
          might not be true at the first tick.
        *)
        let e1,_ = Hept_mapfold.exp_it funs acc e1 in
        let e2,_ = Hept_mapfold.exp_it funs acc e2 in
        if_opt_cond_or_init ~whentrue:e1 ~otherwise:e2 res, acc
    | Eapp({ a_op = Enode _ } as op, e_list, re) ->
        let args,_ = mapfold (Hept_mapfold.exp_it funs) acc e_list in
        let re,_ = optional_wacc (Hept_mapfold.exp_it funs) acc re in
        Eapp(op, args, merge_resets res re), acc
    | Eiterator(it, ({ a_op = Enode _ } as op), n, pe_list, e_list, re) ->
        let pargs,_ = mapfold (Hept_mapfold.exp_it funs) acc pe_list in
        let args,_ = mapfold (Hept_mapfold.exp_it funs) acc e_list in
        let re,_ = optional_wacc (Hept_mapfold.exp_it funs) acc re in
        Eiterator(it, op, n, pargs, args, merge_resets res re), acc
    | Eapp({ a_op = Efun _ } as op, e_list, _) ->
        let args,_ = mapfold (Hept_mapfold.exp_it funs) acc e_list in
        Eapp(op, args, None), acc (* funs don't need resets *)
    | Eiterator(it, ({ a_op = Efun _ } as op), n, pe_list, e_list, _) ->
        let pargs,_ = mapfold (Hept_mapfold.exp_it funs) acc pe_list in
        let args,_ = mapfold (Hept_mapfold.exp_it funs) acc e_list in
        Eiterator(it, op, n, pargs, args, None), acc (* funs don't need resets *)
    | _ -> raise Errors.Fallback

let eq funs (res,_) eq =
  Hept_mapfold.eq funs (res,eq.eq_stateful) eq

let block funs (res,_) b =
  Hept_mapfold.block funs (res,b.b_stateful) b

(* Transform reset blocks in blocks with reseted exps,
   create a var to store the reset condition evaluation if not already a var. *)
let eqdesc funs (res,stateful) = function
  | Ereset(b, ({ e_desc = Evar _ } as e)) ->
        let r = if stateful then merge_resets res (Some e) else res in
        let b, _ = Hept_mapfold.block_it funs (r,stateful) b in
        Eblock(b), (res,stateful)
  | Ereset(b, e) ->
      if stateful then (
        let e, _ = Hept_mapfold.exp_it funs (res,stateful) e in
        let e, vd, eq = reset_var_from_exp e in
        let r = merge_resets res (Some e) in
        let b, _ = Hept_mapfold.block_it funs (r,stateful) b in
        let b = { b with b_equs = eq::b.b_equs; b_local = vd::b.b_local; b_stateful = true } in
        Eblock(b), (res,stateful))
      else ( (* recursive call to remove useless resets *)
        let b, _ = Hept_mapfold.block_it funs (res,stateful) b in
        Eblock(b), (res,stateful))
  | Eautomaton _ | Epresent _ ->
      Format.eprintf "[reset] should be done after [automaton present]";
      assert false
  | _ -> raise Errors.Fallback


let funs = { Hept_mapfold.defaults with Hept_mapfold.eq = eq;
                                        Hept_mapfold.block = block;
                                        Hept_mapfold.eqdesc = eqdesc;
                                        Hept_mapfold.edesc = edesc }

let program p =
  let p, _ = Hept_mapfold.program_it funs (None,true) p in
  p
