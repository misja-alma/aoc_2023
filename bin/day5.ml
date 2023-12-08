open Core
open Aoc
open Range
open Sexp_pretty
open Sexplib
open Sexplib.Std

let file = "input/day5.txt"
let lines = Array.of_list @@ In_channel.read_lines file

let find_index_from p a from =
  let n = Array.length a in
  let rec loop i =
    if i = n then None
    else if p (Array.unsafe_get a i) then Some i
    else loop (succ i) in
  loop from

(* parse seeds
   parse maps individually; seed to soil, soil to fertilizer etc
   define mapping function: input nr, map. 
           for each map row, check if source < nr and source + length > nr. if yes, stop and return target + input - source
            if no match found, return input
   for each seed: pass through list of mappings*)

let parseSeeds str = let (_, nrs) = tuple2_of_list (String.split_on_chars str ~on:[':']) in parseSpaceSeparatedNrs nrs         

let seeds = parseSeeds lines.(0)
let findBlock header lns = 
  let headerIndex = Option.value_exn(find_index_from (fun l -> l = header) lns 0) in 
  let maybeEnder = find_index_from (fun l -> String.length l = 0) lines (headerIndex + 1) in
  let ender = match maybeEnder with 
    | None -> Array.length lines - 1
    | Some(i) -> i - 1 in
  Array.sub lines ~pos:(headerIndex + 1) ~len:(ender - headerIndex)  


let parseMapping lns = lns |> amap parseSpaceSeparatedNrs |> amap tuple3_of_list |> List.of_array

let seedToSoil = parseMapping (findBlock "seed-to-soil map:" lines)
let soilToFertilizer = parseMapping (findBlock "soil-to-fertilizer map:" lines)
let fertilizerToWater = parseMapping (findBlock "fertilizer-to-water map:" lines)
let waterToLight = parseMapping (findBlock "water-to-light map:" lines)
let lightToTemperature = parseMapping (findBlock "light-to-temperature map:" lines)
let temperatureToHumidity = parseMapping (findBlock "temperature-to-humidity map:" lines)
let humidityToLocation = parseMapping (findBlock "humidity-to-location map:" lines)
let mappings = [seedToSoil; soilToFertilizer; fertilizerToWater; waterToLight; lightToTemperature; temperatureToHumidity; humidityToLocation]

let mapSourceLine nr (target, source, len) = if (source < nr) && (source + len > nr) then Some(target + nr - source) else None
let mapSource nr mapping = match List.find_map mapping ~f:(mapSourceLine nr) with
  | Some(found) -> found
  | None -> nr

let mapSeed seed = List.fold mappings ~init:seed ~f:mapSource
let mappedSeeds = lmap mapSeed seeds
let solution = List.min_elt mappedSeeds ~compare:compare

let () = print_endline @@ "Solution 1: " ^ (string_of_int (Option.value_exn solution))

let seedRanges = sliding seeds ~size:2 ~step:2 |> 
                 lmap tuple2_of_list |> 
                 lmap (fun (s, l) -> {start = s; length = l})
(* Result is a list of lists of (source range, offset) *)
let mapRanges = lmap (lmap (fun (target, source, length) -> ({start = source; length = length}, (target - source)))) mappings      

(* returns (unmapped [], mapped [])*)
let mapSourceLineRange r1 (r2, offset) = let toMap = intersect r2 r1 in 
                                         if toMap.length = 0 then ([r1], []) else 
                                          let unMapped = complement r1 toMap in
                                          (unMapped, [{start = toMap.start + offset; length = toMap.length}])

let mapSource seedRanges rangeLines = List.fold rangeLines ~init:(seedRanges, []) ~f:(fun (unMapped, mapped) rangeLine -> 
(* for each rangeLine, try to map so far unmapped seed ranges. Mapped ones will be added to the result, unmapped ones remain *)  
  let (stilUnmapped, nowMapped) = List.fold unMapped ~init:([], []) ~f:(fun (unMapped, mapped2) seed -> 
    let (stilUnmapped2, nowMapped2) = mapSourceLineRange seed rangeLine in (List.append stilUnmapped2 unMapped, List.append nowMapped2 mapped2)) 
  in (stilUnmapped, List.append nowMapped mapped))                                        

let printRanges rs = List.iter rs ~f:(fun r -> print_endline (pretty_string Config.default (sexp_of_range r)))   

let resultMapping = List.fold mapRanges ~init:seedRanges ~f:(fun toMap range -> 
  let (unMapped, mapped) = mapSource toMap range in 
  List.append unMapped mapped)
let lowest = List.min_elt resultMapping ~compare:(fun r1 r2 -> compare r1.start r2.start)

let () = print_endline @@ "Solution 2: " ^ (string_of_int (Option.value_exn lowest).start)
