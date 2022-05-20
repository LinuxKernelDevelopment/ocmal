open OUnit2
open Fifth

let tests =  "test suite for fifith" >::: [
        "empty" >:: (fun _ -> assert_equal 0 (fifth []) ~printer:string_of_int);
        "five" >:: (fun _ -> assert_equal 5 (fifth [1; 2; 3; 4; 5]) ~printer:string_of_int);
        "Descend" >:: (fun _ -> assert_equal [5; 4; 3; 2; 1] (lstDec [1; 2; 3; 4; 5]));
]

let _ = run_test_tt_main tests
