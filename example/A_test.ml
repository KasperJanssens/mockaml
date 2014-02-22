open OUnit

module Behaviour =
  struct
    let has_found_stuff_in_database_ret_val = true
  end
module B_mocked = B_mock.TestStubFunctor(Behaviour)
module A = A.A_functor (B_mocked)

let test () =
  let result = A.function_depending_on_B in
  let () = OUnit.assert_equal "KOEKOEK" result in
  ()



let suite = "A_test">:::[
  "mock_test" >:: test;]

let _ = run_test_tt_main suite
