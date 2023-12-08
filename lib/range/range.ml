open Core
open Sexplib.Std

type range = {
  start: int;
  length: int;
} [@@deriving sexp, eq, show]

let empty_range = {start = 0; length = 0} 
let is_empty r = r.length = 0

let intersect r1 r2 = 
  if is_empty r1 || is_empty r2 then empty_range else
  if r1.start < r2.start then 
    if r1.start + r1.length > r2.start then let preamble = r2.start - r1.start in {start = r2.start; length = min r2.length (r1.length - preamble)}
    else empty_range  
  else if r1.start < (r2.start + r2.length) then 
    let preamble = r1.start - r2.start in {start = r1.start; length = min r1.length (r2.length - preamble)}
  else empty_range

let complement r1 r2 = let is = intersect r1 r2 in
  if is.length = 0 then [r1; r2] else 
    let parts = 
      if r1.start < r2.start then 
        if r1.start + r1.length > r2.start + r2.length then 
          [{start = r1.start; length = r2.start - r1.start}; {start = r2.start + r2.length; length = r1.start + r1.length - (r2.start + r2.length)}]
        else [{start = r1.start; length = r2.start - r1.start}; {start = r1.start + r1.length; length = r2.start + r2.length - (r1.start + r1.length)}]
      else if r2.start + r2.length > r1.start + r1.length then 
              [{start = r2.start; length = r1.start - r2.start}; {start = r1.start + r1.length; length = r2.start + r2.length - r1.start - r1.length}]  
            else 
              [{start = r2.start; length = r1.start - r2.start}; {start = r2.start + r2.length; length = r1.start + r1.length - (r2.start + r2.length)}] 
    in List.filter parts ~f:(fun r -> r.length > 0)

let overlaps source target = source.start <= target.start && source.start + source.length >= target.start + target.length

let union r1 r2 = if r1.start + r1.length = r2.start then [{start = r1.start; length = r1.length + r2.length}]
                  else if r2.start + r2.length = r1.start then [{start = r2.start; length = r1.length + r2.length}]
                  else let is = intersect r1 r2 in
                    if is.length = 0 then [r1; r2] else if overlaps r1 r2 then [r1] else if overlaps r2 r1 then [r2] else [{start = min r1.start r2.start; length = r1.length + r2.length - is.length}]
