open Core

let (=) = Poly.(=)
let (!=) a b = not(a = b)

let rec range from upto = if from = upto then [from] else from :: range (from + 1) upto

let flatten xs = 
  List.concat  @@ List.map xs ~f:(function | None -> [] | Some x -> [x])

let rec intSum xs = match xs with
  | [] -> 0
  | y :: ys -> (y + intSum ys)


