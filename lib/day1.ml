open! Core

module Day1Part1 = struct
  let solve () =
    let lines = In_channel.read_lines "./inputs/day1part1.txt" in
    let lines =
      List.map
        ~f:(fun line ->
          (*The only reason i do this is so that i can use String.split later as it takes only 1 char as delimiter*)
          let line = String.substr_replace_all ~pattern:"   " ~with_:" " line in
          let line = String.split ~on:' ' line in
          let first = List.nth_exn line 0 in
          let second = List.nth_exn line 1 in
          (int_of_string first, int_of_string second))
        lines
    in
    let first, second = List.unzip lines in
    let first = List.sort ~compare:Int.compare first in
    let second = List.sort ~compare:Int.compare second in
    let result =
      List.fold2_exn ~init:0 ~f:(fun acc a b -> acc + abs (a - b)) first second
    in
    printf "%d\n" result
end

module Day1Part2 = struct
  let solve () =
    let lines = In_channel.read_lines "./inputs/day1part2.txt" in
    let table = Hashtbl.create (module Int) in
    let first =
      List.map
        ~f:(fun line ->
          (*The only reason i do this is so that i can use String.split later as it takes only 1 char as delimiter*)
          let line = String.substr_replace_all ~pattern:"   " ~with_:" " line in
          let line = String.split ~on:' ' line in
          let first = int_of_string @@ List.nth_exn line 0 in
          let second = int_of_string @@ List.nth_exn line 1 in
          Hashtbl.set table ~key:second
            ~data:(Hashtbl.find_or_add table second ~default:(fun _ -> 0) + 1);
          first)
        lines
    in
    let result =
      List.fold ~init:0
        ~f:(fun acc el ->
          acc + (el * Hashtbl.find_or_add table el ~default:(fun _ -> 0)))
        first
    in
    printf "%d\n" result
end
