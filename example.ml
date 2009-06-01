(******************************************************************************)
(* A generic pattern-matching verifier based on Luc Maranget's paper.         *)
(* Copyright (c) 2009 Adrien Guatto - Licensed under MIT license.             *)
(******************************************************************************)

open Matching
open Utils

module TEST_SIG =
struct
  type tag = string
  let compare = compare

  type pattern_ast =
    | True | False
    | Or of pattern_ast * pattern_ast
    | Left of pattern_ast | Right of pattern_ast
    | Couple of pattern_ast * pattern_ast
    | First of pattern_ast | Second of pattern_ast
    | Var of string

  let rec inject c = match c with
    | True -> Pconstr ("True", []) | False -> Pconstr ("False", [])
    | Or (l, r) -> Por (inject l, inject r)
    | Left p -> Pconstr ("Left", [inject p])
    | Right p -> Pconstr ("Right", [inject p])
    | First p -> Pconstr ("First", [inject p])
    | Second p -> Pconstr ("Second", [inject p])
    | Couple (l, r) -> Pconstr ("Couple", [inject l; inject r])
    | Var _ -> Pany

  let rec eject c = match c with
    | Por (l, r) -> Or (eject l, eject r)
    | Pconstr ("True", []) -> True | Pconstr ("False", []) -> False
    | Pconstr ("Left", [p]) -> Left (eject p)
    | Pconstr ("Right", [p]) -> Right (eject p)
    | Pconstr ("First", [p]) -> First (eject p)
    | Pconstr ("Second", [p]) -> Second (eject p)
    | Pconstr ("Couple", [cl; cr]) -> Couple (eject cl, eject cr)
    | Pany -> Var "_"
    | _ -> invalid_arg "TEST_SIG.eject"

  let rec pp_pattern_ast p = match p with
    | True -> "True"
    | False -> "False"
    | Or (l, r) -> pp_pattern_ast l ^ "|" ^ pp_pattern_ast r
    | Left p' -> Printf.sprintf "Left %s\n" (pp_pattern_ast p')
    | Right p' -> Printf.sprintf "Right %s\n" (pp_pattern_ast p')
    | First p' -> Printf.sprintf "(fst %s)\n" (pp_pattern_ast p')
    | Second p' -> Printf.sprintf "(snd %s)\n" (pp_pattern_ast p')
    | Couple (l, r) ->
        Printf.sprintf "(%s,%s)" (pp_pattern_ast l) (pp_pattern_ast r)
    | Var x -> x

  let is_complete l =
    match l with
      | ["False"; "True"] -> true
      | ["Couple"] -> true
      | ["Left"; "Right"] -> true
      | ["First"; "Second"] -> true
      | _ -> false

  let not_in l =
    match l with
      | ["True"] -> "False"
      | ["False"] -> "True"
      | _ -> assert false

  let arity c = match c with
    | "True" -> 0
    | "False" -> 0
    | "Couple" -> 2
    | "Left" -> 1
    | "Right" -> 1
    | "Var" -> 0
    | _ -> assert false
end


module TEST_CHECKER = PATTERN_CHECKER (TEST_SIG)

open Printf

open TEST_SIG
open TEST_CHECKER

let _ =
  let p =
    [(Couple (True, Var "x"));
     (Or (Couple (False, True), Couple(False, True)));
     (Couple (True, False))] in
  let r = check p in
  begin match r.not_matched with
    | None -> ()
    | Some p ->
        printf "Warning: this pattern-matching is not exhaustive.\n";
        printf "Here is an example of a value that is not matched:\n";
        printf "%s\n" (pp_pattern_ast p)
  end;
  begin match r.redundant_patterns with
    | [] -> ()
    | l ->
        let f p =
          Printf.printf "Warning: match-case %s is unused.\n"
            (pp_pattern_ast p) in
        List.iter f l
  end
