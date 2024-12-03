open! Core
open! Poly

module Day2Part1 = struct
  let solve () =
    let lines = In_channel.read_lines "./inputs/day2part1.txt" in
    let safe =
      List.fold ~init:0
        ~f:(fun acc line ->
          let entries = String.split line ~on:' ' in
          let entries = List.map entries ~f:int_of_string in
          let sorted_entries = List.sort ~compare:Int.compare entries in
          let sorted_rev = List.rev sorted_entries in
          match entries = sorted_entries || entries = sorted_rev with
          | false -> acc
          | true ->
              let level_differences =
                try
                  List.map2_exn (List.drop_last_exn entries)
                    (List.tl_exn entries) ~f:(fun a b -> abs (a - b))
                with Invalid_argument _ -> []
              in
              let invalid_differences =
                List.filter level_differences ~f:(fun a -> a < 1 || a > 3)
              in
              let count =
                if List.length invalid_differences > 0 then 0 else 1
              in
              acc + count)
        lines
    in
    printf "\n\n%d\n" safe
end

module Day2Part2 = struct
  let is_safe entries =
    let sorted_entries = List.sort ~compare:Int.compare entries in
    let sorted_rev = List.rev sorted_entries in
    match entries = sorted_entries || entries = sorted_rev with
    | false -> false
    | true ->
        let level_differences =
          List.map2_exn (List.drop_last_exn entries) (List.tl_exn entries)
            ~f:(fun a b -> abs (a - b))
        in
        let invalid_differences =
          List.filter level_differences ~f:(fun a -> a < 1 || a > 3)
        in
        List.length invalid_differences = 0

  let rec dampener entries idx =
    let new_entries =
      let hd, tl = List.split_n entries idx in
      let tl = List.tl_exn tl in
      hd @ tl
    in
    match is_safe new_entries with
    | true -> true
    | false ->
        if List.length entries - 1 = idx then false
        else dampener entries (idx + 1)

  let solve () =
    let lines = In_channel.read_lines "./inputs/day2part2.txt" in
    let safe =
      List.fold ~init:0
        ~f:(fun acc line ->
          let entries = String.split line ~on:' ' in
          let entries = List.map entries ~f:int_of_string in
          let count = if is_safe entries || dampener entries 0 then 1 else 0 in
          acc + count)
        lines
    in
    printf "\n\n%d\n" safe
end
