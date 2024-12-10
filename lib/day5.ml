open! Core
open! Poly

module Day5Part1 = struct
  let print_string_list data =
    let pages_str =
      List.fold data ~init:"" ~f:(fun acc v -> sprintf "%s %s" acc v)
    in
    printf "%s\n" pages_str;
    ()

  let print_int_list data =
    let pages_str =
      List.fold data ~init:"" ~f:(fun acc v -> sprintf "%s %d" acc v)
    in
    printf "%s\n" pages_str;
    ()

  let process_mappings lines =
    let tbl = Hashtbl.create (module String) in
    List.iter lines ~f:(fun line ->
        let parts = String.split line ~on:'|' in
        let key = List.nth_exn parts 0 in
        let value = List.nth_exn parts 1 in
        let values = Hashtbl.find_or_add tbl key ~default:(fun () -> []) in
        Hashtbl.set tbl ~key ~data:(value :: values));
    tbl

  let process_updates lines = List.map lines ~f:(String.split ~on:',')

  let process_input filename =
    let lines = In_channel.read_lines filename in
    let sep_idx, _ = List.findi_exn lines ~f:(fun _ v -> v = "") in
    let mapping, updates = List.split_n lines (sep_idx + 1) in
    let mapping = List.drop_last_exn mapping in

    let mapping = process_mappings mapping in
    let updates = process_updates updates in
    (mapping, updates)

  let sort_pages mapping update =
    List.sort update ~compare:(fun a b ->
        let after_a = Hashtbl.find_exn mapping a in
        let after_b = Hashtbl.find_exn mapping b in

        match List.find after_a ~f:(fun a -> a = b) with
        | Some _ -> -1
        | None -> (
            match List.find after_b ~f:(fun b -> b = a) with
            | Some _ -> 1
            | None -> 0))

  let solve () =
    let mapping, updates = process_input "./inputs/day5part1.txt" in
    let valid_updates =
      List.filter updates ~f:(fun pages ->
          let sorted = sort_pages mapping pages in
          pages = sorted)
    in
    let result =
      List.fold valid_updates ~init:0 ~f:(fun acc pages ->
          let mid_idx = ((List.length pages + 1) / 2) - 1 in
          let mid_page = List.nth_exn pages mid_idx in
          acc + int_of_string mid_page)
    in
    printf "\n%d\n" result
end

module Day5Part2 = struct
  let process_mappings lines =
    let tbl = Hashtbl.create (module String) in
    List.iter lines ~f:(fun line ->
        let parts = String.split line ~on:'|' in
        let key = List.nth_exn parts 0 in
        let value = List.nth_exn parts 1 in
        let values = Hashtbl.find_or_add tbl key ~default:(fun () -> []) in
        Hashtbl.set tbl ~key ~data:(value :: values));
    tbl

  let process_updates lines = List.map lines ~f:(String.split ~on:',')

  let process_input filename =
    let lines = In_channel.read_lines filename in
    let sep_idx, _ = List.findi_exn lines ~f:(fun _ v -> v = "") in
    let mapping, updates = List.split_n lines (sep_idx + 1) in
    let mapping = List.drop_last_exn mapping in

    let mapping = process_mappings mapping in
    let updates = process_updates updates in
    (mapping, updates)

  let sort_pages mapping update =
    List.sort update ~compare:(fun a b ->
        let after_a = Hashtbl.find_exn mapping a in
        let after_b = Hashtbl.find_exn mapping b in

        match List.find after_a ~f:(fun a -> a = b) with
        | Some _ -> -1
        | None -> (
            match List.find after_b ~f:(fun b -> b = a) with
            | Some _ -> 1
            | None -> 0))

  let find_invalid_pages mapping pages =
    let sorted = sort_pages mapping pages in
    pages <> sorted

  let fix_updates mapping update =
    List.sort update ~compare:(fun a b ->
        let after_a = Hashtbl.find_exn mapping a in
        let after_b = Hashtbl.find_exn mapping b in

        match List.find after_a ~f:(fun a -> a = b) with
        | Some _ -> -1
        | None -> (
            match List.find after_b ~f:(fun b -> b = a) with
            | Some _ -> 1
            | None -> 0))

  let solve () =
    let mapping, updates = process_input "./inputs/day5part2.txt" in
    let invalid_updates = List.filter updates ~f:(find_invalid_pages mapping) in
    let fixed_updates = List.map invalid_updates ~f:(fix_updates mapping) in
    let result =
      List.fold fixed_updates ~init:0 ~f:(fun acc pages ->
          let mid_idx = ((List.length pages + 1) / 2) - 1 in
          let mid_page = List.nth_exn pages mid_idx in
          acc + int_of_string mid_page)
    in
    printf "\n%d\n" result
end
