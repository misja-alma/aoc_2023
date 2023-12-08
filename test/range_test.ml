open OUnit2
open Range

let tests = "test suite for range" >::: [
  "basic test" >:: (fun _ -> assert_equal 2 ({start = 1; length = 2}.length));
]

let _ = run_test_tt_main tests