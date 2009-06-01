What?
*****

ML-like languages offer a powerful construct called pattern-matching, allowing a
programmer to safely and efficiently deconstruct structured data. This project
offers a simple and generic core that implements two useful checks for
pattern-matching:

- exhaustiveness := detecting when a pattern-matching construct does not check
for all possibilities, e.g. @match var with | true -> [...]@.

- usefulness := reporting clauses subsumed by previous ones, like the second 0
in @match var with | 0 -> [...]  | 0 -> [...] | _ -> [...]@.

It is based on Luc Maranget's paper "Warnings for Pattern Matching". Curious
readers should visit his home page for more information

How?
****

You should copy the files to your source directory. Do not forget utils.ml, or
add its content to your home-made utility functions' file.

A client language only has to provide a module of signature Matching.SIG with
appropriate functions (basically, translations to and from the module's internal
representation).

A small example is available in example.ml. To build, just run @ocamlbuild
example.byte@.

Why?
****

This tiny module was originally developed for and is currently used in a
fully fledged compiler for a new functional language.

Adrien Guatto

[ This work was sponsored by the French Institut National de Recherche en
Informatique et Automatique (National Institute for Research in Computer and
Control Sciences)'s action Synchronics. ]