open OUnit2
open ListSet

let tests = "test suite for ListSet" >::: [
  "mem" >:: (fun _ -> assert_equal (ListSet.add 1 ListSet.empty |> ListSet.add 2 |> ListSet.mem 1) true);
]

let _ = run_test_tt_main tests
