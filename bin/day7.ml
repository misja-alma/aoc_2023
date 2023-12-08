open Core
open Aoc

let file = "input/day7.txt"
let lines = In_channel.read_lines file

(* split hands in hand bid *)
(* for each hand: toList, sort, determine kind, store in type *)
(* sort hands + bids based on hand ranking *)
(* map h,b,i -> i*b; sum result *)

type ranking = HighCard | OnePair | TwoPair | Trips | FullHouse | Carre | Poker [@@deriving enum]

(* requires that h is sorted *)
let rankHand jokerRules h = let groups = List.group h ~break:(fun a b -> not (a = b)) in 
                 let groupLengths = if not jokerRules then  List.map groups ~f:List.length else
                    (* If any J's are present, add them to the largest group first *) 
                    let maybeJokerGroup = List.partition_tf groups ~f:(fun gr -> List.hd_exn gr = 'J') in match maybeJokerGroup with
                                          | ([], _) -> List.map groups ~f:List.length
                                          | (jokerGroup, others) -> let nrJokers = List.length(List.hd_exn jokerGroup) in
                                                                    let otherGroupLengths = List.sort (List.map others ~f:List.length) ~compare:(Fn.flip compare) in
                                                                    if nrJokers = 5 then [5] else (List.hd_exn otherGroupLengths + nrJokers) :: (List.tl_exn otherGroupLengths) in                                                                   
                  let sortedGroupLenghts = List.sort groupLengths ~compare:compare in match sortedGroupLenghts with
                    | [5] -> Poker
                    | [1;4] -> Carre
                    | [2;3] -> FullHouse
                    | [1;1;3] -> Trips
                    | [1;2;2] -> TwoPair
                    | [1;1;1;2] -> OnePair
                    | [1;1;1;1;1] -> HighCard
                    | _ -> failwith @@ "Impossible groups " ^ (String.of_char_list h)   

type hand = {
  rank: ranking;
  cards: char list;
}    

let cardRank jokerRules c = if Char.is_digit c then int_of_char c - 48 else 
  match Char.uppercase c with 
    | 'T' -> 10
    | 'J' -> if jokerRules then 1 else 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "Unknown card!"
 
let compareCard jokerRules c1 c2 = compare (cardRank jokerRules c1) (cardRank jokerRules c2)

let compareCards jokerRules cs1 cs2 = 
  let rec compareZippedCards zs = match zs with 
    | [] -> let () = print_endline "Same cards!" in 0
    | (c1,c2) :: cs -> let cResult = compareCard jokerRules c1 c2 in
                         if cResult <> 0 then cResult else compareZippedCards cs in
  compareZippedCards @@ List.zip_exn cs1 cs2                      

let compareHands jokerRules h1 h2 = let rankCompare = compare (ranking_to_enum h1.rank) (ranking_to_enum h2.rank) in
                         if rankCompare <> 0 then rankCompare 
                         else compareCards jokerRules h1.cards h2.cards   

let parseHand jokerRules h = 
  let sorted = h |> String.to_list |> 
  fun h -> List.sort h ~compare:(Fn.flip (compareCard jokerRules)) in { rank = rankHand jokerRules sorted; cards = String.to_list h }    

let handsWithBids jokerRules = 
  List.map lines ~f:(fun l -> l |> 
  fun l -> String.split_on_chars l ~on:[' '] |> 
  tuple2_of_list |> 
  fun (h, bid) -> (parseHand jokerRules h, int_of_string bid))    

let sortedHands jokerRules = List.sort (handsWithBids jokerRules) ~compare:(fun (h1, _) (h2, _) -> compareHands jokerRules h1 h2)

let valued jokerRules = List.mapi (sortedHands jokerRules) ~f:(fun index (_, bid) -> (index + 1) * bid)

let solution = intSum (valued false)

let () = print_endline @@ "Solution 1: " ^ (string_of_int solution)
  

let solution = intSum (valued true)

let () = print_endline @@ "Solution 2: " ^ (string_of_int solution)                 