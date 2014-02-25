open OUnit

module Behaviour =
  struct
    let has_found_stuff_in_database_ret_val = true
    let three_arg_func_ret_val = "Mockingbird"
  end
module B_mocked = B_mock.TestStubFunctor(Behaviour)
module A = A.A_functor (B_mocked)

let test () =
  let result = A.function_depending_on_B in
  let () = OUnit.assert_equal "KOEKOEK" result in
  ()

let test_two_arguments () =
  let ()= A.call_func_with_three_args_twice in
  let () = B_mocked.assert_three_arg_func_interactions 2 in
  let () = B_mocked.assert_three_arg_func_arg 1 (1,false) in
  let () = B_mocked.assert_three_arg_func_arg 2 (2,true) in
  ()

let test_failure_two_arguments () =
  let ()= A.call_func_with_three_args_twice in
  let () =
    try
      B_mocked.assert_three_arg_func_interactions 3
    with
    | Failure _s -> ()
    | _ -> OUnit.assert_failure "expected failure for assertion of number of interactions"
  in
  let () =
    try
      B_mocked.assert_three_arg_func_arg 1 (1,true)
    with
    | Failure _s -> ()
    | _ -> OUnit.assert_failure "expected not found for arguments"
  in
  ()

let suite = "A_test">:::[
  "mock_test" >:: test;
  "test_two_arguments" >:: test_two_arguments ;
  "test_failure_two_arguments" >:: test_failure_two_arguments ;
]

let _ = run_test_tt_main suite
