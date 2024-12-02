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
