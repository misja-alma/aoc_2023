open Core
open Aoc 
open Sexp_pretty
open Sexplib
open Sexplib.Std

let file = "/home/almam/ocaml/aoc_2023/input/day3.txt"
let lines = In_channel.read_lines file

let width = String.length (List.hd_exn lines)
let height = List.length lines
let rows = range 0 (height - 1)
let cols = range 0 (width - 1)
let grid = Array.make_matrix ~dimx:width ~dimy:height '.'

let set (m: 'a array array) (x: int) (y:int) (v: 'a) : unit =
  m.(y).(x) <- v

let () = 
for y = 0 to (height -1 ) do
  for x = 0 to (width - 1)  do
    set grid x y (String.nget (List.nth_exn lines y) x)
  done
done    


(* initialise the lines into an array *)
(* read each row from left to right. Look for digits. Once found, initialise a nr list and keep adding digits, also for each digit, check neighbours for symbol 
   mark a flag if a symbol is found *)
(* filter the list on symbol found, calculate the numbers, sum *)    

type point = {
  x: int;
  y: int;
} [@@deriving sexp]

module OrderedPoint =
struct
  type t = point [@@deriving sexp] (* God knows why this is needed here *)

  let compare a b = 
    if a.x = b.x then compare a.y b.y
    else compare a.x b.x 
end

module PS = Set.Make(OrderedPoint)

type number_block = {
  digits: char list; (* Note: reversed! *)
  symbols: PS.t;
} [@@deriving sexp] 


let neighbours ~x ~y ~width ~height = 
  let all = 
    [{x = x-1; y = y-1}; {x = x; y = y-1}; {x = x+1; y = y-1};
    {x = x-1; y = y}; {x = x+1; y = y};
    {x = x-1; y = y+1}; {x = x; y = y+1}; {x = x+1; y = y+1}] in
  List.filter all ~f:(fun pt -> pt.x >= 0 && pt.y >= 0 && pt.x < width && pt.y < height)  

let get grid pt = grid.(pt.y).(pt.x)  

let neighbour_symbols row col = let nbs = neighbours ~x:col ~y:row ~width:width ~height:height in
                               let result = List.filter nbs ~f:(fun pt -> let value = get grid pt in (not (Char.is_digit value)) && (value != '.')) in
                               PS.of_list result

let checkCell state row col = let c = grid.(row).(col) in 
  if Char.is_digit c then 
    match state with
      | (Some(block), bs) -> let new_digits = c :: block.digits in 
                             let new_symbols = Set.union (neighbour_symbols row col) block.symbols  in
                             (Some {digits = new_digits; symbols = new_symbols }, bs)
      | (None, bs) -> let new_block = {digits = [c]; symbols = neighbour_symbols row col } in (Some(new_block), bs)  
  else match state with 
      | (Some(block), bs) -> (None, block :: bs)
      | _ -> state

let getNrsInRow total row = let (maybeLastBlock, result) = List.fold cols ~init:(None, []) ~f:(fun state col -> checkCell state row col) in 
  match maybeLastBlock with 
    | Some(block) -> List.append total (block :: result)
    | None -> List.append result total

let nrs = List.fold rows ~init:[] ~f:getNrsInRow  

(* TODO find out why here we need to use Set.is_empty and e.g above use PS in PS.of_list *)
let with_symbol = List.filter nrs ~f:(fun block -> not(Set.is_empty block.symbols))
let digitsAsNr digits = digits |> List.rev |> String.of_char_list |> int_of_string
let as_nr = List.map with_symbol ~f:(fun block -> digitsAsNr block.digits)
let solution = intSum as_nr

let () = print_endline @@ "Solution 1: " ^ (string_of_int solution)


(* for part 2, go through the list and collect a list of possible gears. Then for each possible gear, collect their connected numbers *)
(* filter that list for gears with exactly 2 numbers. Map the list to the product of the numbers, solution = sum *)  

let stars symbols = Set.filter symbols ~f:(fun s -> (get grid s) = '*')
let allStars = let starSet = List.fold nrs ~init:PS.empty ~f:(fun total block -> Set.union total (stars block.symbols)) in Set.to_list starSet
let starsWithCounts = List.map allStars ~f:(fun star -> let starOwners = List.filter nrs ~f:(fun block -> Set.mem block.symbols star) in (star, starOwners))
let gearsWithOwners = List.filter starsWithCounts ~f:(fun (_, owners) -> List.length owners = 2)
let gearRatios = List.map gearsWithOwners ~f:(fun (_, owners) -> List.fold owners ~init:1 ~f:(fun total b -> total * (digitsAsNr b.digits)))
let solution2 = intSum gearRatios

let () = print_endline @@ "Solution 2: " ^ (string_of_int solution2)
