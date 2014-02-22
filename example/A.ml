module type B_sig =
  sig
    val has_found_stuff_in_database:string -> bool
  end

module A_functor = functor (B:B_sig) ->
struct

let handle_stuff_found stuff =
  String.uppercase stuff

let function_depending_on_B =
  let stuff = "Koekoek" in
  if B.has_found_stuff_in_database stuff then
    handle_stuff_found stuff
  else
    raise Not_found
end


include A_functor(B)

