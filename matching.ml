open Utils

(********************************************************************)
(* Syntax for patterns, and pretty-printers                         *)
(********************************************************************)

(** Generic pattern, basically constructors with holes and alternation,
    tagged with any type. *)
type 'a pattern =
  | Any
  | Or of 'a pattern * 'a pattern
  | Constr of 'a * 'a pattern list
and 'a patt_vec = 'a pattern list
(* Row vectors *)
and 'a patt_matrix = 'a patt_vec list

(********************************************************************)
(* Module type for constructor signatures                           *)
(********************************************************************)

(** Module type for pattern signatures. *)
module type SIG =
sig
  type tag
  val compare : tag -> tag -> int
  val arity : tag -> int
  val is_complete : tag list -> bool
  val not_in : tag list -> tag

  type pattern_ast
  val inject : pattern_ast -> tag pattern
  val eject : tag pattern -> pattern_ast

  val pp_pattern_ast : pattern_ast -> string

end

(********************************************************************)
(* The algorithm itself, parametrized over signatures               *)
(********************************************************************)

module PATTERN_CHECKER = functor (S : SIG) ->
struct
  module SSet = Set.Make(struct
                           type t = S.tag
                           let compare = S.compare
                         end)
  let uniq l = SSet.elements (List.fold_right SSet.add l SSet.empty)

  let is_complete sigma = S.is_complete (uniq sigma)
  let not_in sigma = S.not_in (uniq sigma)

  let rec head_constrs h = match h with
    | Constr (c, q) -> [(c, List.length q)]
    | Or (l, r) -> head_constrs l @ head_constrs r
    | Any -> []

  let rec matS c ar p =
    let vecS pv = match pv with
      | [] -> assert false
      | Constr (c', r') :: pv' -> if c = c' then [r' @ pv'] else []
      | Any :: pv' -> [repeat ar Any @ pv']
      | Or (t1, t2) :: pv' -> matS c ar [t1 :: pv'; t2 :: pv'] in
    List.concat (List.map vecS p)

    let rec matD p =
      let vecD pv = match pv with
        | Constr _ :: _ -> []
        | Any :: pv' -> [pv']
        | Or (t1, t2) :: pv' -> matD [t1 :: pv'; t2 :: pv']
        | _ -> assert false in
      List.concat (List.map vecD p)

    let rec algU p q =
      match (p, q) with
        | ([], _) -> true       (* p has no lines *)
        | (_ :: _, []) -> false (* p has no columns *)

        | (h :: t,  Constr (c, r) :: q') ->
            let p' = matS c (List.length r) p in
            algU p' (r @ q')

        | (h :: t, Or (r1, r2) :: q') ->
            algU p (r1 :: q') || algU p (r2 :: q')

        | (h :: t, Any :: q') ->
            let sigma =
              List.concat (List.map (fun v -> head_constrs (List.hd v)) p) in
            let algU_constr (c_k, ar_k) =
              let p' = matS c_k ar_k p in
              algU p' (repeat ar_k Any @ q') in
            let sigma_used = List.exists algU_constr sigma in
            sigma_used || (if not (is_complete (List.map fst sigma))
                           then algU (matD p) q' else false)


    type 'a trivec = { p : 'a patt_vec;
                       q : 'a patt_vec;
                       r : 'a patt_vec }
    and 'a trimat = 'a trivec list


    let rec trimatS c arity mv =
      let filter_line l = match l.p with
          | Constr (c', t) :: p' ->
              if c = c' then [{ l with p = t @ p' }] else []
          | Any :: p' ->
              [{ l with p = repeat arity Any @ p' }]
          | Or (t1, t2) :: p' ->
              trimatS c arity [{ l with p = t1 :: p' }; { l with p = t2 :: p' }]
          | _ -> assert false in
      List.concat (List.map filter_line mv)

    let shift1 l = match l.p with
      | p :: p' -> { l with p = p'; q = p :: l.q }
      | _ -> assert false

    let shift2 l = match l.p with
      | p :: p' -> { l with p = p'; r = p :: l.r }
      | _ -> assert false

    let simple_union e e' = match (e, e') with
      | (Some l, Some l') -> Some (l @ l')
      | (None, _) | (_, None) -> None

    let explode n m =
      let l = List.map (cut_at n) m in
      (List.map (fun (b, _, a) -> b @ a) l,
       List.map (fun (_, x, _) -> [x]) l)

    let rec (@&) m1 m2 = match (m1, m2) with
      | ([], []) -> []
      | (h1 :: t1, h2 :: t2) -> (h1 @ h2) :: (m1 @& m2)
      | _ -> invalid_arg "@&"

    let union r1 r2 e' e'' = match (e', e'') with
      | (Some [], Some []) -> Some []
      | (None, None) -> None
      | (Some [], None) -> Some [r2]
      | (None, Some []) -> Some [r1]

      | (Some [], Some (_ :: _)) -> e''
      | (Some (_ :: _), Some []) -> e'

      | (None, Some ((_ :: _) as t)) -> Some (r1 :: t)
      | (Some ((_ :: _) as t), None) -> Some (r2 :: t)

      | (Some ((_ :: _) as t'), Some ((_ :: _) as t'')) -> Some (t' @ t'')

    let rec algU' m v =
      match v.p with
          (* Phase one *)
        | Constr (c, t) :: p' ->
            algU' (trimatS c (List.length t) m) { v with p = t @ p' }
        | Any :: _ ->
            algU' (List.map shift1 m) (shift1 v)
        | Or _ :: _ ->
            algU' (List.map shift2 m) (shift2 v)
        | [] ->
            (* Phase two *)
            begin match v.r with
              | [] ->
                  let qm = List.map (fun l -> l.q) m in
                  if algU qm v.q then Some [] else None
              | _ :: _ ->
                  let rec compute_Ej j =
                    begin match List.nth v.r j with
                      | Or (t1, t2) ->
                          let f l =
                            let r_j = keep l.r j
                            and r_woj = drop l.r j in
                            { p = [r_j]; q = r_woj @ l.q; r = [] } in
                          let rv_woj = drop v.r j in
                          let m' = List.map f m in
                          let m'' =
                            m' @ [{ p = [t1]; q = drop v.r j @ v.q; r = [] }] in
                          let r1 = algU' m'
                            { p = [t1]; q = rv_woj @ v.q; r = [] }
                          and r2 = algU' m''
                            { p = [t2]; q = rv_woj @ v.q; r = [] } in
                          union t1 t2 r1 r2
                      | _ -> assert false
                    end in
                  let j_list = range 1 (List.length (List.hd m).r) in
                  let computed_Ej = List.map compute_Ej j_list in
                  List.fold_left simple_union (Some []) computed_Ej
            end

    let disp_list l = List.map (fun lp -> S.pp_pattern_ast (S.eject lp)) l

    let rec algI m n i =
      Printf.printf "%salgI> %d\n" (String.make i ' ') n;
      List.iter (fun l -> Printf.printf "%s%s\n"
                   (String.make i ' ')
                   (concat_with " | " (disp_list l))) m;
      let r = begin
      match (m, n) with
      | ([], 0) -> Some []
      | ([] :: _, 0) -> None
      | (m, n) ->
          let sigma =
            List.concat (List.map (fun v -> head_constrs (List.hd v)) m) in
          let sigma_c = List.map fst sigma in
          let default =
            if is_complete sigma_c
            then None
            else algI (matD m) (n - 1) (i + 1) in
          begin match default with
            | Some p ->
                begin match sigma with
                  | [] -> Some (Any :: p)
                  | _ :: _ ->
                      let c' = not_in sigma_c in
                      Some (Constr (c', repeat (S.arity c') Any) :: p)
                end
            | None ->
                let rec traverse_sigma sigma = match sigma with
                  | [] -> None
                  | (c, ar) :: sigma' ->
                      let res =
                        algI (matS c ar m) (ar + n - 1) (i + 1) in
                      begin match res with
                        | None -> traverse_sigma sigma'
                        | Some v ->
                            let (r, p) = separate ar v in
                            Some (Constr (c, r) :: p)
                      end in
                traverse_sigma sigma
          end
      end in
      Printf.printf "%s=> %s\n"
        (String.make i ' ')
        (match r with
           | None -> "None"
           | Some l -> concat_with " | " (disp_list l));
      r

    type result = { not_matched : pattern_ast option;
                    redundant_patterns : pattern_ast list; }

    let check_pattern_matching m =
      let m' = List.map (fun v -> List.map S.inject v) m in
      match m' with
        | [] -> invalid_arg "check_matching"
        | v :: _ ->
            let n = List.length v in
            begin match algI m' n 0 with
              | None -> ()
              | Some vr ->
                  Printf.printf "Warning: non-exhaustive pattern-matching.\n";
                  Printf.printf "Not matched: %s\n" (concat_with " | "
                                                       (disp_list vr))
            end;
            let make_trivec v = { p = v; q = []; r = [] } in
            let make_trimat m = List.map make_trivec m in
            let string_of_patt p = S.pp_pattern_ast (S.eject p) in
            let check_line m v =
              let r = algU' (make_trimat m) (make_trivec v) in
              begin match r with
                | Some [] -> ()
                | Some r ->
                    Printf.printf "Warning: unused patterns:\n";
                    let rs : string list = List.map string_of_patt r in
                    Printf.printf "(%s)\n" (concat_with "|" rs)
                | None ->
                    let rs = List.map string_of_patt v in
                    Printf.printf "Warning: unused pattern %s.\n"
                      (concat_with "|" rs)
              end;
              m @ [v] in
            ignore (List.fold_left check_line [(List.hd m')] (List.tl m'))
end

(* let ident fmt s = Format.sprintf fmt "%s" s *)

(* let rec pp_pattern fmt c = match c with *)
(*   | Any -> ident fmt "_" *)
(*   | Or (l, r) -> *)
(*       Format.fprintf fmt "@[(%a@ |@ %a)@]" pp_pattern l pp_pattern r *)
(*   | Constr (c, l) -> *)
(*       let rec pp fmt l = match l with *)
(*         | [] -> ident fmt "" *)
(*         | [e] -> Format.fprintf fmt "%a" pp_pattern e *)
(*         | h :: t -> Format.fprintf fmt "%a,@ %a" pp_pattern h pp t in *)
(*       Format.fprintf fmt "@[%s(%a)@]" c pp l *)
(* and pp_pattern' c = pp_pattern Format.std_formatter c *)

