open Core

let (=) = Poly.(=)
let (!=) a b = not(a = b)

let rec range from upto = if from = upto then [from] else from :: range (from + 1) upto

let rec intSum xs = match xs with
  | [] -> 0
  | y :: ys -> (y + intSum ys)

let tuple2_of_list = function
  | [a;b] -> (a, b)
  | _ -> failwith "List expected to have exactly 2 elements"


let lmap f xs = List.map xs ~f:f  
let lbind f xs = List.concat_map xs ~f:f
let flatten xs = List.concat_map xs ~f:(function | None -> [] | Some x -> [x])

