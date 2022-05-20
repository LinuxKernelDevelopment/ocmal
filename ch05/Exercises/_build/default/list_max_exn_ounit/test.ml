open OUnit2
open Max

let tests = "test suite for list_max_exn" >::: [
        "empty" >:: (fun _ -> assert_equal "empty" (list_max []));
        "test1" >:: (fun _ -> assert_equal "3" (list_max [1;2;3]));
        "test2" >:: (fun _ -> assert_equal "876" (list_max [1;876;63;87]));
]

let _ = run_test_tt_main tests
