open! Core

module Parent_and_child = struct
  type t =
    { parent : Entry.t
    ; child : Entry.t
    }
  [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

type t =
  { index : Entry.Set.t Fact.Base.Table.t
  ; facts : Fact.Set.t Entry.Table.t
  ; fields : Entry.t String.Map.t Entry.Table.t
  ; enums : Entry.t String.Map.t Entry.Table.t
  ; field_pairs : Parent_and_child.Set.t String.Table.t
  ; enum_pairs : Entry.Set.t String.Table.t
  }

let create () =
  { index = Fact.Base.Table.create ()
  ; facts = Entry.Table.create ()
  ; fields = Entry.Table.create ()
  ; enums = Entry.Table.create ()
  ; field_pairs = String.Table.create ()
  ; enum_pairs = String.Table.create ()
  }
;;
