open Core
open Angstrom
open Aoc

let (=) = Poly.(=);;


let file = "input/day2.txt"
let lines = In_channel.read_lines file

(* let lines = ["Game 1: 4 red, 5 blue, 4 green; 1 red, 1 blue, 2 green; 9 blue, 6 red; 1 green, 3 red, 7 blue; 3 green, 7 red";
"Game 2: 2 blue, 1 green, 2 red; 1 red, 2 green, 2 blue; 1 red, 1 green, 1 blue; 7 green, 1 blue"];; *)

(* Game 3: 3 green, 4 red; 10 red, 2 blue, 5 green; 9 red, 3 blue, 5 green *)

(* TODO parse lines into map of color -> nr, map with line nr, filter lines that exceed condition, map to nr, sum *)
(* a line is a ; separated sequence of moves. A move is a , separated sequence of color-nr. Parse all moves, then add up their color nrs *)

let eval (str:string) expr =
  match parse_string ~consume:All expr str with
  | Ok v      -> v
  | Error msg -> failwith msg;;


let is_whitespace = function
| '\x20' | '\x0a' | '\x0d' | '\x09' -> true
| _ -> false

let whitespace = take_while is_whitespace

let is_digit = function '0'..'9' -> true | _ -> false

let number = take_while1 is_digit >>| int_of_string

let color_string = string "red" <|> string "green" <|> string "blue"

let color = whitespace *> number >>= fun nr -> return nr *> whitespace *> color_string >>= fun clr -> return (clr, nr)

let parseMoves str = let colors = String.split_on_chars str ~on:[','] in
   List.map colors ~f:(fun clr -> eval clr color)

let game line = let parts = String.split_on_chars line ~on:[':'] in 
  let prefix = List.hd_exn parts in
  let remainder = List.hd_exn @@ List.tl_exn parts in
  let nr = int_of_string @@ String.sub prefix ~pos:5 ~len:(String.length prefix - 5) in
  let mvs = List.map (String.split_on_chars remainder ~on:[';']) ~f:parseMoves in
  (nr, mvs)

type color_list = (string * int) list

let zeroTotals: color_list = [("red", 0); ("green", 0); ("blue", 0)]

let maxTotals (totals:color_list) (mv:(color_list)) = List.map totals ~f:(fun (clr, total) -> let matched = List.find mv ~f:(fun c -> fst c = clr) in 
  match matched with 
    | Some((c, nr)) -> (clr, max total nr)
    | None -> (clr, total)  
  )

let maxColorsInMove (mvs:color_list list) = List.fold_left mvs ~init:zeroTotals  ~f:maxTotals

let maxColors (nr, moveList) = let sums = maxColorsInMove moveList in (nr, sums)
  
let totalsUsed = let games = List.map lines ~f:game in List.map games ~f:maxColors  

let inUse = [("red", 12); ("green", 13); ("blue", 14)]

let possible = List.filter totalsUsed ~f:(fun (_, totals) -> List.for_all totals ~f:(fun (clr, total) -> total <= snd @@ List.find_exn inUse ~f:(fun (c, _) -> c = clr)))

let solution = intSum (List.map possible ~f:fst)

let () = print_endline @@ "Solution part 1: " ^ (string_of_int solution)

let powers = List.map totalsUsed ~f:(fun (_, totals) -> List.fold_left totals ~init:1 ~f:(fun total (_, nr) -> total * nr))

let solution2 = intSum powers

let () = print_endline @@ "Solution part 2: " ^ (string_of_int solution2)