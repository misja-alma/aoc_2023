open Core

let file = "input/day1.txt"
let lines = In_channel.read_lines file

let firstLast s = 
  let res = if String.length s = 1 then (s ^ s) 
  else if String.length s > 2 then Base.String.of_char_list [s.[0]; s.[String.length s - 1]] 
  else s
  in let () = print_endline res in res 

let digits = List.map lines (fun line -> String.filter line Char.is_digit)
let sum = List.sum (module Int) digits ~f:(fun s -> s |> firstLast |> int_of_string)

let () = print_endline ("Solution: " ^ string_of_int sum)