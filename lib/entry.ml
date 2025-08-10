open! Core

type t = int [@@deriving sexp, compare, hash]

include functor Comparable.Make
include functor Hashable.Make
