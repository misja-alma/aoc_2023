open Core
open Aoc

let file = "input/day4.txt"
let lines = In_channel.read_lines file

let pow2 n = Int.shift_left 1 n

module IS = Set.Make(Int)

let parseLine line = let nrs = List.nth_exn (String.split_on_chars line ~on:[':']) 1 in
                     let parts = String.split_on_chars nrs ~on:['|'] in
                     parts |> 
                     lmap parseSpaceSeparatedNrs |> 
                     lmap (IS.of_list) |> 
                     tuple2_of_list

let nrSets = List.map lines ~f:parseLine
let winners = List.map nrSets ~f:(fun (targets, nrs) -> Set.inter targets nrs)   
let nrWinners = List.map winners ~f:Set.length
let powers = List.map nrWinners ~f:(fun w -> if w = 0 then 0 else pow2 (w - 1))     
let solution = intSum powers

let () = print_endline @@ "Solution 1: " ^ (string_of_int solution)


let nrCards = List.length nrWinners
let cardCounts = Array.create ~len:nrCards 1

let () =
  for c = 0 to (nrCards-1) do
    let wins = List.nth_exn nrWinners c in
    let ownCount = cardCounts.(c) in
      
    for c2 = (c + 1) to (min (nrCards - 1) (c + wins)) do
      cardCounts.(c2) <- (cardCounts.(c2) + ownCount)
    done  
  done

let solution2 = intSum (List.of_array cardCounts)  
let () = print_endline @@ "Solution 2: " ^ (string_of_int solution2)

                     