open Matching
open Utils

module TEST_SIG =
struct
  type tag = string
  let compare = compare

  type pattern_ast =
    | True | False
    | Int of int
    | Left of pattern_ast | Right of pattern_ast
    | Couple of pattern_ast * pattern_ast
    | First of pattern_ast | Second of pattern_ast
    | Var of string

  let rec inject c = match c with
    | True -> Constr ("True", []) | False -> Constr ("False", [])
    | Int i -> Constr (string_of_int i, [])
    | Left p -> Constr ("Left", [inject p])
    | Right p -> Constr ("Right", [inject p])
    | First p -> Constr ("First", [inject p])
    | Second p -> Constr ("Second", [inject p])
    | Couple (l, r) -> Constr ("Couple", [inject l; inject r])
    | Var _ -> Any

  let raise_exc f e = try ignore (f e); false with _ -> true

  let rec eject c = match c with
    | Constr (s, []) when not (raise_exc int_of_string s) ->
        Int (int_of_string s)
    | Constr ("True", []) -> True | Constr ("False", []) -> False
    | Constr ("Left", [p]) -> Left (eject p)
    | Constr ("Right", [p]) -> Right (eject p)
    | Constr ("First", [p]) -> First (eject p)
    | Constr ("Second", [p]) -> Second (eject p)
    | Constr ("Couple", [cl; cr]) -> Couple (eject cl, eject cr)
    | Any -> Var "_"
    | _ -> invalid_arg "TEST_SIG.eject"

  type ty = | Bool | Prod of ty * ty | Sum of ty * ty

  let rec pp_pattern_ast p = match p with
    | True -> "True"
    | False -> "False"
    | Int i -> string_of_int i
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
    (* match ty with *)
    (*   | Bool -> List.length l >= 2 *)
    (*   | Sum _ -> List.length l >= 2 *)
    (*   | Prod _ -> true *)

  let not_in l =
    Printf.printf "not_in [%s]\n" (concat_with ";" l);
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
  check_pattern_matching [[Couple (True, Var "x")]; [Couple (False, True)]];
  (* check_pattern_matching [[True]]; *)

