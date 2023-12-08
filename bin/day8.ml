open Core
open Aoc

let file = "input/day8.txt"
let lines = In_channel.read_lines file

let directions = String.to_array @@ List.hd_exn lines

let parseWay line = 
  let (from, toward) = tuple2_of_list @@ List.map (String.split_on_chars line ['=']) String.strip in
  let towards = tuple2_of_list @@ List.map (String.split_on_chars (String.slice toward 1 (String.length toward - 1)) [',']) String.strip in
  (from, towards)


let ways = List.map (List.drop lines 2) ~f:parseWay

let waysMap = Map.of_alist_exn (module String) ways
let start = ref "AAA"
let count = ref 0
let directionPos = ref 0

let () = 
  while not (!start = "ZZZ") do 
      let direction = directions.(!directionPos) in
      begin
        directionPos := (!directionPos + 1) mod (Array.length directions); 
        count := !count + 1;
        let (left, right) = Map.find_exn waysMap (!start) in
        begin 
           let _ = match direction with 
            | 'L' -> start := left;
            | 'R' -> start := right;
            | _ -> failwith "Impossible direction"
          in ()  
        end  
      end
  done    


let () = print_endline @@ "Solution 1: " ^ (count |> (!) |> string_of_int)

let lastChar s = String.get s (String.length s - 1)

let starts = ways |> lfilter (fun (k, _) -> lastChar k = 'A') |> lmap fst

let oneStep directionPos startPos = 
  let direction = directions.(directionPos) in
            let (left, right) = Map.find_exn waysMap startPos in
              let newStart = match direction with 
                | 'L' -> left;
                | 'R' -> right;
                | _ -> failwith "Impossible direction"
              in ((directionPos + 1) mod (Array.length directions), newStart)  

(* Assumes that we are never at an ending poinnt immediately. I.e. count >= 1*)
let pathLength dPos startPos = 
  begin
    let (d, s) = oneStep dPos startPos in
    let start = ref s in
    let count = ref 1 in
    let directionPos = ref d in
    let () = 
      while not (lastChar !start = 'Z') do 
          let (d, s) = oneStep !directionPos !start in
          directionPos := d;
          count := !count + 1;
          start := s; 
      done in 
    (!count, !start)  
end  

let initials = starts |> lmap (pathLength 0)
let initialPathLenghts = List.map initials ~f:fst

let seconds = List.map initials ~f:(fun (c, s) -> pathLength (c mod (Array.length directions)) s)
let secondPathLengths = List.map seconds ~f:fst (* turns out its the same as initialPathLengths already *)

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)

let rec lGcd = function 
  | [] -> failwith "List should not be empty"
  | [x] -> x
  | x :: ys -> gcd x (lGcd ys)

let pathGcd = lGcd secondPathLengths
let offset = List.hd_exn secondPathLengths mod pathGcd  
let factors = List.map secondPathLengths ~f:(fun p -> p / pathGcd)

let solution2 = intProduct factors + offset
 
let () = print_endline @@ "Solution 2: " ^ (string_of_int solution2)
