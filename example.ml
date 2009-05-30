open Matching
open Utils

module TEST_SIG =
struct
  type tag = string
  let compare = compare

  type pattern_ast =
    | True | False
    | OrA of pattern_ast * pattern_ast
    | Left of pattern_ast | Right of pattern_ast
    | Couple of pattern_ast * pattern_ast
    | First of pattern_ast | Second of pattern_ast
    | Var of string

  let rec inject c = match c with
    | True -> Constr ("True", []) | False -> Constr ("False", [])
    | OrA (l, r) -> Or (inject l, inject r)
    | Left p -> Constr ("Left", [inject p])
    | Right p -> Constr ("Right", [inject p])
    | First p -> Constr ("First", [inject p])
    | Second p -> Constr ("Second", [inject p])
    | Couple (l, r) -> Constr ("Couple", [inject l; inject r])
    | Var _ -> Any

  let rec eject c = match c with
    | Or (l, r) -> OrA (eject l, eject r)
    | Constr ("True", []) -> True | Constr ("False", []) -> False
    | Constr ("Left", [p]) -> Left (eject p)
    | Constr ("Right", [p]) -> Right (eject p)
    | Constr ("First", [p]) -> First (eject p)
    | Constr ("Second", [p]) -> Second (eject p)
    | Constr ("Couple", [cl; cr]) -> Couple (eject cl, eject cr)
    | Any -> Var "_"
    | _ -> invalid_arg "TEST_SIG.eject"

  let rec pp_pattern_ast p = match p with
    | True -> "True"
    | False -> "False"
    | OrA (l, r) -> pp_pattern_ast l ^ "|" ^ pp_pattern_ast r
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
     (OrA (Couple (False, True), Couple(False, True)));
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
