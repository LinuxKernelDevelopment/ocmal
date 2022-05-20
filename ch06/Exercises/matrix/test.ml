open OUnit2
open Matrix

let tests = "test suite for matrix" >::: [
        "empty" >:: (fun _ -> assert_equal false (is_valid_matrix []));
        "unequal" >:: (fun _ -> assert_equal false (is_valid_matrix [[1; 2]; [3]]));
        "legal" >:: (fun _ -> assert_equal true (is_valid_matrix [[1; 1; 1]; [9; 8; 7]]))
]

let _ = run_test_tt_main tests
