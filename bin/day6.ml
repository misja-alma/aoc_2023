open Core
open Aoc
open Poly (* Restores polymorphic operators which were overridden by Base *)

let file = "input/day6.txt"
let lines = In_channel.read_lines file

let times = let nrs = List.nth_exn (String.split_on_chars (List.nth_exn lines 0) ~on: [':']) 1 in parseSpaceSeparatedNrs nrs
let distances = let nrs = List.nth_exn (String.split_on_chars (List.nth_exn lines 1) ~on: [':']) 1 in parseSpaceSeparatedNrs nrs
let timesWithDistances = List.zip_exn times distances

(* dist = (t-p)*p; -p^2 + pt > d => p^2 - pt + d < 0 *)
(* 0 points:  p = (t +/- D)/2; D = sqrt(t^2 - 4d ) *)
(* e.g t = 7, d = 9: D = sqrt13; p = (7 + sqrt13)/2 or (7 - sqrt13)/2 = 1.x or 5.x. So round the lower solution up, round the upper solution down *)

let getNrWins t d = let d = sqrt(float_of_int(t*t - 4*d)) in
                    if d < 0.0 then 0 else 
                      let lowSolution = int_of_float @@ Float.round_up @@ (float_of_int t -. d) /. 2.0 in
                      let highSolution = int_of_float @@  (float_of_int t +. d) /. 2.0 in
                      if highSolution < lowSolution then 0 else highSolution - lowSolution + 1


let wins = List.map timesWithDistances ~f:(fun (time, distance) -> getNrWins time distance)
let solution = intProduct wins

let () = print_endline @@ "Solution 1: " ^ (string_of_int solution)

let time2 = let asStrings = lmap string_of_int times in String.concat asStrings
let distance2 = let asStrings = lmap string_of_int distances in String.concat asStrings

let solution2 = getNrWins (int_of_string time2) (int_of_string distance2)

let () = print_endline @@ "Solution 2: " ^ (string_of_int solution2)