open! Core

type 'a t' =
  | Is_entry of Entry.t
  | Has_field of string * 'a
  | Is_enum of (string * 'a) list
  | Is_subtype of 'a
  | Is_supertype of 'a
[@@deriving sexp, variants, hash, compare]

module Base = struct
  type t = Entry.t t' [@@deriving sexp, hash, compare]

  include functor Comparable.Make
  include functor Hashable.Make
end

type t = Fix of t t' [@@deriving sexp, hash, compare]

let of_base = function
  | Is_entry e -> Fix (Is_entry e)
  | Has_field (s, e) -> Fix (Has_field (s, Fix (Is_entry e)))
  | Is_subtype e -> Fix (Is_subtype (Fix (Is_entry e)))
  | Is_supertype e -> Fix (Is_supertype (Fix (Is_entry e)))
  | Is_enum l ->
    Fix (Is_enum (List.map l ~f:(fun (s, e) -> s, Fix (Is_entry e))))
;;

let to_base (Fix t') =
  let entry_of = function
    | Fix (Is_entry e) -> Some e
    | _ -> None
  in
  match t' with
  | Is_entry e -> Some (Is_entry e)
  | Has_field (s, t1) -> Option.map (entry_of t1) ~f:(fun e -> Has_field (s, e))
  | Is_subtype t1 -> Option.map (entry_of t1) ~f:(fun e -> Is_subtype e)
  | Is_supertype t1 -> Option.map (entry_of t1) ~f:(fun e -> Is_supertype e)
  | Is_enum xs ->
    xs
    |> List.map ~f:(fun (s, t1) -> Option.map (entry_of t1) ~f:(fun e -> s, e))
    |> Option.all
    |> Option.map ~f:(fun xs' -> Is_enum xs')
;;

module Query = struct
  type t =
    { entry : Entry.t list
    ; has_field : (string * t) list
    ; is_enum : (string * t) list list
    ; is_subtype : t list
    ; is_supertype : t list
    }
  [@@deriving sexp, hash, compare, fields]
end

let rec to_query (Fix t') =
  match t' with
  | Is_entry e ->
    { Query.entry = [ e ]
    ; has_field = []
    ; is_enum = []
    ; is_subtype = []
    ; is_supertype = []
    }
  | Has_field (s, t1) ->
    { Query.entry = []
    ; has_field = [ s, to_query t1 ]
    ; is_enum = []
    ; is_subtype = []
    ; is_supertype = []
    }
  | Is_enum xs ->
    { Query.entry = []
    ; has_field = []
    ; is_enum = [ List.map xs ~f:(Tuple2.map_snd ~f:to_query) ]
    ; is_subtype = []
    ; is_supertype = []
    }
  | Is_subtype t1 ->
    { Query.entry = []
    ; has_field = []
    ; is_enum = []
    ; is_subtype = [ to_query t1 ]
    ; is_supertype = []
    }
  | Is_supertype t1 ->
    { Query.entry = []
    ; has_field = []
    ; is_enum = []
    ; is_subtype = []
    ; is_supertype = [ to_query t1 ]
    }
;;

include functor Comparable.Make
include functor Hashable.Make
