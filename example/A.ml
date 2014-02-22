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
end


include A_functor(B)

