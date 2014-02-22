open OUnit

let test () = Pervasives.print_string "koekoek"


let suite = "A_test">:::[
  "mock_test" >:: test;]

"ocaml" >::: [suite;]
