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

module Fact_and_entry = struct
  type t =
    { fact : Fact.t
    ; entry : Entry.t
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

module Query = struct
  type t =
    { query : Fact.Query.t
    ; entry : Entry.t
    }
  [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

module Query_result = struct
  type 'a t =
    | No_match
    | Waiting
    | Found of 'a
end

module Session = struct
  type nonrec t =
    { fact_db : t
    ; memoized_satisfies : Entry.Hash_set.t Fact.Table.t
    ; satisfies_queue : Fact_and_entry.t Queue.t
    ; precedent_queries : Fact_and_entry.Set.t Fact_and_entry.Table.t
    ; added_to_queue : Fact_and_entry.Hash_set.t
    ; and_state : Fact_and_entry.Set.t Fact_and_entry.Table.t
    ; or_state : Fact_and_entry.Set.t Fact_and_entry.Table.t
    }

  let lookup_satisfies_fact t ~fact ~entry =
    match Fact.to_base fact with
    | Some base ->
      (match Hashtbl.find t.fact_db.index base with
       | None -> false
       | Some entries -> Set.mem entries entry)
    | None ->
      (match Hashtbl.find t.memoized_satisfies fact with
       | None -> false
       | Some memo -> Hash_set.mem memo entry)
  ;;

  let start t ~precedent ~fact entry_opt : Entry.t Query_result.t =
    match entry_opt with
    | None -> No_match
  ;;

  let satisfies_fact_inner t ~fact ~entry : Entry.t Query_result.t =
    if lookup_satisfies_fact t ~fact ~entry
    then Found entry
    else (
      let (Fix fact) = fact in
      match fact with
      | Is_entry e when Entry.equal e entry -> Found entry
      | Is_entry _ -> No_match
      | Has_field (s, fact') ->
        Hashtbl.find t.fact_db.fields entry
        |> Option.bind ~f:(fun fields -> Map.find fields s)
        |> start t ~precedent:{ Fact_and_entry.fact; entry } ~fact:fact')
  ;;
end

type in_progress_query =
  { possible_entries : Entry.Set.t
  ; to_check : Fact.t list
  ; query : Fact.Query.t
  }

(* let query t ~(query : Fact.Query.t) = *)
(*   let q = Queue.create () in *)
(*   let process fact = *)
(*     match Fact.to_base fact with *)
(*     | None -> () *)
(*     | Some base -> *)
(*       Hashtbl.find t.index base *)
(*       |> Option.iter ~f:(Hash_set.iter ~f:(Queue.enqueue q)) *)
(*   in *)
(*   () *)
(* ;; *)
