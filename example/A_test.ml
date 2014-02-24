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

let test_twee_argumenten () =
  let ()= A.call_func_with_three_args_twice in
  let () = B_mocked.assert_three_arg_func_interactions 2 in
  ()


let suite = "A_test">:::[
  "mock_test" >:: test;
  "test_twee_argumenten" >:: test_twee_argumenten ;
]

let _ = run_test_tt_main suite
