open! Core
open! Poly

module Day4Part1 = struct
  let pattern = String.to_array "XMAS"
  let pattern_start = Array.get pattern 0
  let _inc_idxs inc_i inc_j i j = (i + inc_i, j + inc_j)
  let inc_top_left = _inc_idxs (-1) (-1)
  let inc_top_line = _inc_idxs (-1) 0
  let inc_top_right = _inc_idxs (-1) 1
  let inc_left = _inc_idxs 0 (-1)
  let inc_right = _inc_idxs 0 1
  let inc_bot_left = _inc_idxs 1 (-1)
  let inc_bot_line = _inc_idxs 1 0
  let inc_bot_right = _inc_idxs 1 1

  let inc_fns =
    [
      inc_top_left;
      inc_top_line;
      inc_top_right;
      inc_left;
      inc_right;
      inc_bot_left;
      inc_bot_line;
      inc_bot_right;
    ]

  let count_matches_in_dir mat i j dir_fn =
    let rec count mat i j dir_fn char_idx =
      match char_idx = Array.length pattern with
      | true -> 1
      | false -> (
          let new_i, new_j = dir_fn i j in
          match (new_i, new_j) with
          | x, y
            when x < 0 || y < 0
                 || x >= Array.length mat
                 || y >= Array.length (Array.get mat 0) ->
              0
          | _ -> (
              let exp_char = Array.get pattern char_idx in
              let act_char = Array.get (Array.get mat new_i) new_j in
              match exp_char = act_char with
              | true -> count mat new_i new_j dir_fn (char_idx + 1)
              | false -> 0))
    in
    (* NOTE: here we already know that X matches -> start from 1 *)
    count mat i j dir_fn 1

  let count_matches mat i j =
    let rec count mat i j dir_fns num_matches =
      match dir_fns with
      | fn :: dir_fns ->
          let found_matches = count_matches_in_dir mat i j fn in
          count mat i j dir_fns (num_matches + found_matches)
      | [] -> num_matches
    in

    count mat i j inc_fns 0

  let solve () =
    let lines = In_channel.read_lines "./inputs/day4part1.txt" in
    let lines = Array.of_list lines in
    let mat = Array.map lines ~f:String.to_array in

    let result =
      Array.foldi ~init:0
        ~f:(fun i acc line ->
          let occurrences =
            Array.foldi ~init:0
              ~f:(fun j acc ch ->
                let count =
                  if ch = pattern_start then count_matches mat i j else 0
                in
                acc + count)
              line
          in
          acc + occurrences)
        mat
    in
    printf "\n%d\n" result
end

module Day4Part2 = struct
  let solve () =
    let result = 5 in
    printf "\n%d\n" result
end
