open! Core

module Day3Part1 = struct
  let solve () =
    let txt = In_channel.read_all "./inputs/day3part1.txt" in
    let re_pattern = Re2.create_exn {|mul\((\d{1,3},\d{1,3})\)|} in
    let matches = Re2.find_all_exn ~sub:(`Index 1) re_pattern txt in
    let result =
      List.fold matches ~init:0 ~f:(fun acc m ->
          let a, b =
            match String.split m ~on:',' with
            | [ a; b ] -> (int_of_string a, int_of_string b)
            | _ -> failwith "wrong format"
          in
          acc + (a * b))
    in
    printf "\n%d\n" result
end

module Day3Part2 = struct
  let solve () =
    let txt = In_channel.read_all "./inputs/day3part1.txt" in
    let re_pattern = Re2.create_exn {|mul\((\d{1,3},\d{1,3})\)|} in
    let matches = Re2.find_all_exn ~sub:(`Index 1) re_pattern txt in
    let result =
      List.fold matches ~init:0 ~f:(fun acc m ->
          let a, b =
            match String.split m ~on:',' with
            | [ a; b ] -> (int_of_string a, int_of_string b)
            | _ -> failwith "wrong format"
          in
          acc + (a * b))
    in
    printf "\n%d\n" result
end
