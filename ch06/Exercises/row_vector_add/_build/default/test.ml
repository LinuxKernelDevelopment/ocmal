open OUnit2
open Vectoradd

let tests = "test suite for add_row_vectors" >::: [
        "normal" >:: (fun _ -> assert_equal [10; 9; 8] (add_row_vectors [1; 1; 1] [9; 8; 7]))
]

let _ = run_test_tt_main tests
