open OUnit2
open Range

let range13 = {start = 1; length = 3}
let range24 = {start = 2; length = 3}
let range04 = {start = 0; length = 5}
let rangeOutside = {start = 10; length = 2}
let emptyRange = {start = 1; length = 0}

let assertEquals expected actual _ = assert_equal expected actual ~printer:Range.show_range

let tests = "test suite for range" >::: [
  "intersect 1" >:: assertEquals {start = 2; length = 2} (Range.intersect range13 range24);
  "intersect 2" >:: assertEquals range13 (Range.intersect range13 range04);
  "intersect 3" >:: assertEquals Range.empty_range (Range.intersect range13 rangeOutside);
  "intersect 1 reverse" >:: assertEquals {start = 2; length = 2} (Range.intersect range24 range13);
  "intersect 2 reverse" >:: assertEquals range13 (Range.intersect range04 range13);
  "intersect 3 reverse" >:: assertEquals Range.empty_range (Range.intersect rangeOutside range13);
  "intersect empty 1" >:: assertEquals Range.empty_range (Range.intersect emptyRange range24);
  "intersect empty 2" >:: assertEquals Range.empty_range (Range.intersect emptyRange range04);
]

let _ = run_test_tt_main tests