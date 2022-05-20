open OUnit2
open MulList

let tests = "test suite for product" >::: [
        "empty" >:: (fun _ -> assert_equal 1 (mulList []));
        "five" >:: (fun _ -> assert_equal 120 (mulList [1; 2; 3; 4; 5]));
]

let _ = run_test_tt_main tests
