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

module Fact_pattern = struct
  type t =
    | Entry of Entry.t
    | Entries of Entry.t list
    | Enum_fields of (string * Entry.t list) list
    | Or of t list
    | And of t list
  [@@deriving sexp, variants, hash, compare]
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
    ; results : Fact_pattern.t Query_result.t Fact_and_entry.Table.t
    ; queue : Fact_and_entry.t Queue.t
    ; precedent_queries : Fact_and_entry.Set.t Fact_and_entry.Table.t
    ; and_state : Fact_and_entry.t list Fact_and_entry.Table.t
    ; or_state : Fact_and_entry.t list Fact_and_entry.Table.t
    }

  let lookup t ~fact ~entry : Fact_pattern.t Query_result.t option =
    match Fact.to_base fact with
    | Some base ->
      (match Hashtbl.find t.fact_db.index base with
       | None -> None
       | Some entries ->
         if Set.mem entries entry
         then Some (Found (Entry entry))
         else Some No_match)
    | None -> Hashtbl.find t.results { fact; entry }
  ;;

  let enqueue t fact_and_entry =
    if not (Hashtbl.mem t.results fact_and_entry)
    then (
      Queue.enqueue t.queue fact_and_entry;
      Hashtbl.set t.results ~key:fact_and_entry ~data:Waiting)
  ;;

  let add_precedent t ~descendant ~precedent =
    Hashtbl.update t.precedent_queries descendant ~f:(function
      | None -> Fact_and_entry.Set.singleton precedent
      | Some set -> Set.add set precedent)
  ;;

  let iff t ~precedent ~fact entry_opt : Fact_pattern.t Query_result.t =
    match entry_opt with
    | None -> No_match
    | Some entry ->
      let descendant : Fact_and_entry.t = { fact; entry } in
      add_precedent t ~descendant ~precedent;
      enqueue t descendant;
      Waiting
  ;;

  let iff_list t ~creator ~precedent fact_and_entries
    : Fact_pattern.t Query_result.t
    =
    match fact_and_entries with
    | [] -> Found (creator [])
    | fact_and_entries ->
      Hashtbl.add_exn t.or_state ~key:precedent ~data:fact_and_entries;
      List.iter fact_and_entries ~f:(fun descendant ->
        add_precedent t ~descendant ~precedent;
        enqueue t descendant);
      Waiting
  ;;

  let and_ t ~precedent fact_and_entries : Fact_pattern.t Query_result.t =
    iff_list
      t
      ~creator:(fun l -> Fact_pattern.And l)
      ~precedent
      fact_and_entries
  ;;

  let or_ t ~precedent fact_and_entries : Fact_pattern.t Query_result.t =
    iff_list t ~creator:(fun l -> Fact_pattern.Or l) ~precedent fact_and_entries
  ;;

  let satisfies_fact_inner t ~fact ~entry : Fact_pattern.t Query_result.t =
    match lookup t ~fact ~entry with
    | Some result -> result
    | None ->
      let (Fix fact') = fact in
      (match fact' with
       | Is_supertype _ | Is_subtype _ -> failwith "TODO"
       | Is_entry e when Entry.equal e entry -> Found (Entry entry)
       | Is_entry _ -> No_match
       | Has_field (s, f) ->
         Hashtbl.find t.fact_db.fields entry
         |> Option.bind ~f:(fun fields -> Map.find fields s)
         |> iff t ~precedent:{ Fact_and_entry.fact; entry } ~fact:f
       | Is_enum l ->
         Hashtbl.find t.fact_db.enums entry
         |> Option.bind ~f:(fun enums ->
           List.map l ~f:(fun (s, fact) ->
             Map.find enums s
             |> Option.map ~f:(fun entry -> { Fact_and_entry.fact; entry }))
           |> Option.all
           |> Option.map ~f:(or_ t ~precedent:{ Fact_and_entry.fact; entry }))
         |> Option.map ~f:(Fn.const Query_result.Waiting)
         |> Option.value ~default:Query_result.No_match)
  ;;
end

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
