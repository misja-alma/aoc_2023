include Binary_heap
include Hashtbl
open Sexp_pretty
open Sexplib
open Sexplib.Std

type ops = Plus | Minus | Mul | Div [@@deriving sexp]
let all_opps = [Plus; Minus; Mul; Div]

type step = {
  left: int;
  op: ops;
  right: int;
} [@@deriving sexp]

type countle_state = {
  subtotal: int; (* Note: better name would be latestResult *)
  nrsLeft: int list;
  (* TODO Do we want to cache the hashcode of the steps? *)
  steps: step list;
} [@@deriving sexp]

let calculate {left; right; op} = match op with
    Plus  -> Some(left + right)
  | Minus -> Some(left - right)
  | Mul   -> Some(left * right)
  | Div   -> if right == 0 || (left mod right != 0) then None else Some(left / right)

let rec remove xs x = match xs with
    [] -> failwith "x not found in list"
  | y :: ys -> if x == y then ys else y :: remove ys x 

let applyOp state step = 
  (* Note: what do we do with negative results? And with zeroes? *)
  match calculate step with (* TODO use map *)
      None -> None
    | Some(result) ->  
        let newNrsLeft = result :: remove (remove state.nrsLeft step.left) step.right in
        let newSteps = step :: state.steps in
          Some { subtotal = result; nrsLeft = newNrsLeft; steps = newSteps }

let take2 lst = 
  let (let*) xs f = List.concat_map f xs in
    let* x1 = lst in
    let* x2 = lst in
    List.filter (fun (a,b) -> a != b) [(x1, x2)] 

let possibleOps state = 
  let (let*) xs f = List.concat_map f xs in
    let* operands = take2 state.nrsLeft in
    List.map (fun op -> { left = fst operands; right = snd operands; op = op } ) all_opps
    

let listCompare l1 l2 = Int.compare (Hashtbl.hash l1) (Hashtbl.hash l2)

(* needed for Set *)
module OrderedCountleState =
struct
  type t = countle_state
  let compare x y = 
    if x.subtotal = y.subtotal then listCompare x.nrsLeft y.nrsLeft
    else compare x.subtotal y.subtotal 
end

module CS = Set.Make(OrderedCountleState)
let visited = CS.empty

let q = Queue.create()

(* Algo; while queue not empty and not found, take new element. Check if visited or found. If not, add to visited, create children, filter on visited, add all. *)

let inputs = Sys.argv
let total = int_of_string inputs.(1)
let nrs = List.map int_of_string (Array.to_list (Array.sub inputs 2 (Array.length inputs - 2)))

let state = { subtotal = 0; nrsLeft = nrs; steps = [] }
let () = Queue.add state q

let found = ref false
let result = ref state (* TODO check if something like a Nil ref exists *)

let () =
  while not (Queue.is_empty q) && not !found do 
    let next = Queue.pop q in
      begin
        let () = found := (next.subtotal == total) in
        begin
          if !found then result := next
          else
            begin
              if not (CS.mem next visited) then 
               begin
                ignore @@ CS.add next visited;
                let possible = possibleOps next in
                let nextStates = List.concat_map (fun x -> x |> (applyOp next) |> Option.to_list) possible in
                let newNextStates = List.filter (fun x -> not (CS.mem x visited)) nextStates in
                begin
                  ignore @@ Queue.add_seq q (List.to_seq newNextStates);
                end    
              end  
            end
        end
      end
  done 


let () = if !found then
  begin
    let () = print_endline "Found solution:" in
      print_endline @@ pretty_string Config.default (sexp_of_countle_state !result)
  end  
else print_endline "No solution found"