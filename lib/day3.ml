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
  let re_pattern = Re2.create_exn {|^mul\((\d{1,3},\d{1,3})\)|}

  let find_match_pos txt idx pattern =
    match String.substr_index ~pos:idx txt ~pattern with
    | Some m -> m
    | None -> -1

  type match_t = Mul of int * int | Do | Dont [@@deriving show]

  let extract_mul txt idx =
    let sub_txt = String.slice txt idx (String.length txt - 1) in
    let m = Or_error.ok @@ Re2.find_first ~sub:(`Index 1) re_pattern sub_txt in
    let a, b =
      match m with
      | Some sub -> (
          match String.split sub ~on:',' with
          | [ a; b ] -> (int_of_string a, int_of_string b)
          | _ -> failwith "wrong format")
      | None -> (0, 0)
    in
    Mul (a, b)

  let find_matches txt =
    let rec find txt idx matches =
      let mul_match = find_match_pos txt idx "mul(" in
      let do_match = find_match_pos txt idx "do()" in
      let dont_match = find_match_pos txt idx "don't()" in
      match (mul_match, do_match, dont_match) with
      | -1, -1, -1 -> matches
      | x, -1, -1 -> find txt (x + 1) (matches @ [ extract_mul txt x ])
      | -1, x, -1 -> find txt (x + 1) (matches @ [ Do ])
      | -1, -1, x -> find txt (x + 1) (matches @ [ Dont ])
      | a, b, -1 ->
          if a < b then find txt (a + 1) (matches @ [ extract_mul txt a ])
          else find txt (b + 1) (matches @ [ Do ])
      | -1, a, b ->
          if a < b then find txt (a + 1) (matches @ [ Do ])
          else find txt (b + 1) (matches @ [ Dont ])
      | a, -1, b ->
          if a < b then find txt (a + 1) (matches @ [ extract_mul txt a ])
          else find txt (b + 1) (matches @ [ Dont ])
      | a, b, c ->
          if a < b && a < c then
            find txt (a + 1) (matches @ [ extract_mul txt a ])
          else if b < a && b < c then find txt (b + 1) (matches @ [ Do ])
          else find txt (c + 1) (matches @ [ Dont ])
    in

    find txt 0 []

  let solve () =
    let txt = In_channel.read_all "./inputs/day3part2.txt" in
    let matches = find_matches txt in
    let _, result =
      List.fold matches ~init:(true, 0) ~f:(fun (allowed, sum) m ->
          match m with
          | Mul (a, b) ->
              if allowed then (allowed, sum + (a * b)) else (allowed, sum)
          | Do -> (true, sum)
          | Dont -> (false, sum))
    in
    printf "\n%d\n" result
end
