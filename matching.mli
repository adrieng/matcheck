(******************************************************************************)
(* A generic pattern-matching verifier based on Luc Maranget's paper.         *)
(* Copyright (c) 2009 Adrien Guatto - Licensed under MIT license.             *)
(******************************************************************************)

(** Generic representation of patterns used by the algorithm. *)
type 'a pattern =
  | Pany
  | Por of 'a pattern * 'a pattern
  | Pconstr of 'a * 'a pattern list

(** This signature describes the pattern-matching part of your language. *)
module type SIG =
  sig
    (** Tag for the constructed patterns. *)
    type tag
    (** Order relation used for tag sorting. *)
    val compare : tag -> tag -> int
    (** Number of sub-patterns that a given pattern tag accepts. *)
    val arity : tag -> int
    (** {i is_complete tag_list} should return true when {i tag_list} forms a
        complete signature. {i tag_list} is sorted and its elements are
        guaranteed to be unique. *)
    val is_complete : tag list -> bool
    (** {i not_in tag_list} should return a well-typed constructor tag that is
        not in the incomplete {i tag_list}. *)
    val not_in : tag list -> tag
    (** Your language's type for patterns. *)
    type pattern_ast
    (** Translate a pattern from your language to a generic one. *)
    val inject : pattern_ast -> tag pattern
    (** Translate a generic pattern obtained from {i inject} to your language's
        patterns. *)
    val eject : tag pattern -> pattern_ast
  end

module PATTERN_CHECKER :
  functor (S : SIG) ->
    sig
      (** Results from {i check}. *)
      type result = {
        not_matched : S.pattern_ast option; (** Any value not matched. *)
        redundant_patterns : S.pattern_ast list; (** Useless patterns. *)
      }
      (** {i check pattern_list} checks for exhaustiveness and usefulness
          of {i pattern_list}. *)
      val check : S.pattern_ast list -> result
    end
