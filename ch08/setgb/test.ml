open OUnit2
open ListSet

let tests = "test suite for ListSet" >::: [
  "mem" >:: (fun _ -> assert_equal (ListSet.add 1 ListSet.empty |> ListSet.add 2 |> ListSet.mem 1) true);
  "elements" >:: (fun _ -> assert_equal (ListSet.add 1 ListSet.empty |> ListSet.add 2 |> ListSet.elements) [1; 2]);
  "UniqueMem" >:: (fun _ -> assert_equal (UniqListSet.add 1 UniqListSet.empty |> UniqListSet.add 2 |> UniqListSet.mem 1) true);
  "UniqueElements" >:: (fun _ -> assert_equal (UniqListSet.add 1 UniqListSet.empty |> UniqListSet.add 2 |> UniqListSet.elements) [2; 1]);
  "UniqueElements" >:: (fun _ -> assert_equal (UniqListSet.add 1 UniqListSet.empty |> UniqListSet.add 2 |> UniqListSet.add 1 |> UniqListSet.elements) [2; 1]);
]

let _ = run_test_tt_main tests
