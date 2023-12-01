open Core

let file = "input/day1.txt"
let lines = In_channel.read_lines file

let rec range from upto = if from = upto then [from] else from :: range (from + 1) upto
let flatten xs = 
  List.concat  @@ List.map xs ~f:(function | None -> [] | Some x -> [x])

let tokens = List.zip_exn (range 0 9) ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
let digits = let ds = range 0 9 in List.zip_exn ds (List.map ds ~f:string_of_int)
let numbers = List.append tokens digits

let rec index_of xs x = match xs with
    [] -> None
  | y :: ys -> if x = y then Some(0) else Option.map (index_of ys x) ~f:(fun n -> n + 1) 

let firstMatch line nr = Option.map (nr |> snd |> (String.substr_index_all line ~may_overlap:false) |> List.hd) (fun x -> (fst nr, x))
let lastMatch line nr = Option.map (nr |> snd |> (String.substr_index_all line ~may_overlap:false) |> List.last) (fun x -> (fst nr, x))

let findFirsts line = flatten @@ List.map numbers ~f:(firstMatch line)
let findLasts line = flatten @@ List.map numbers ~f:(lastMatch line)

let firstLast line = 
  let firsts = findFirsts line in
  let lasts = findLasts line in
  let bestFirst = List.min_elt firsts ~compare:(fun a b -> compare (snd a) (snd b)) in
  let bestLast = List.max_elt lasts ~compare:(fun a b -> compare (snd a) (snd b)) in 
  (fst @@ Option.value_exn bestFirst, fst @@ Option.value_exn bestLast)


let firstLasts = List.map lines ~f:firstLast
let sum = List.sum (module Int) firstLasts ~f:(fun (first, last) -> first * 10 + last)

let () = print_endline ("Solution: " ^ string_of_int sum)