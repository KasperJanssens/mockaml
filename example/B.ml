module B =
  struct
    let has_found_stuff_in_database _stuff =
      true
   let three_arg_func  _int _bool =
     "koekoek"
  end

include B
