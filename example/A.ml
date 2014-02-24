module A_functor = functor (B:B_sig.B_sig) ->
struct

let handle_stuff_found stuff =
  String.uppercase stuff

let function_depending_on_B=
  let stuff = "Koekoek" in
  let result = if (B.has_found_stuff_in_database stuff) then
    handle_stuff_found stuff
  else
    raise Not_found
  in
  result

let call_func_with_three_args_twice =
  let arg1_1 = 1 in
  let arg2_1 = false in
  let _result1 = B.three_arg_func arg1_1 arg2_1 in
  let arg1_2 = 2 in
  let arg2_2 = true in
  let _result2 = B.three_arg_func arg1_2 arg2_2 in
  ()

end


include A_functor(B)

