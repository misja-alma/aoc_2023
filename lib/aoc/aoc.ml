open Core

let (=) = Poly.(=)
let (!=) a b = not(a = b)

let rec range from upto = if from = upto then [from] else from :: range (from + 1) upto

let rec intSum xs = match xs with
  | [] -> 0
  | y :: ys -> (y + intSum ys)
  
let rec intProduct xs = match xs with
  | [] ->  failwith "List expected with at least 1 element"  
  | [x] -> x
  | y :: ys -> (y * intProduct ys)  

let rec bigIntProduct xs = match xs with
  | [] ->  failwith "List expected with at least 1 element"  
  | [x] -> Big_int_Z.big_int_of_int x
  | y :: ys -> (Big_int_Z.mult_big_int (Big_int_Z.big_int_of_int y) (bigIntProduct ys)) 

let tuple2_of_list = function
  | [a;b] -> (a, b)
  | _ -> failwith "List expected to have exactly 2 elements"

let tuple3_of_list = function
  | [a;b;c] -> (a, b, c)
  | _ -> failwith "List expected to have exactly 3 elements"  


let parseSpaceSeparatedNrs str = let rawInts = List.filter (String.split_on_chars str ~on:[' ']) ~f:(fun t -> String.length t > 0) in
  List.map rawInts ~f:int_of_string


let lmap f xs = List.map xs ~f:f  
let lbind f xs = List.concat_map xs ~f:f
let lfilter f xs = List.filter xs ~f:f
let flatten xs = List.concat_map xs ~f:(function | None -> [] | Some x -> [x])
(* requires step <= size *)
let rec sliding xs ~size ~step = let () = assert (step <= size) in
  if List.length xs <= size then [xs] else let (head, tail) = (List.take xs size, List.drop xs step) in head :: sliding tail ~size:size ~step:step


let amap f xs = Array.map xs ~f:f  
let abind f xs = Array.concat_map xs ~f:f

