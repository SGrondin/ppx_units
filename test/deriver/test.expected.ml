module Foo =
  struct
    type t =
      {
      some_int_field: int [@unit "ABC"];
      some_string_field: string ;
      some_other_field: string [@units.unit "GHI"]}[@@deriving units]
    include
      struct
        type some_int_field =
          | ABC 
        type some_string_field =
          | SOME_STRING_FIELD 
        type some_other_field =
          | GHI 
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
let abc : Foo.some_int_field = ABC
let def : Foo.some_string_field = Foo.SOME_STRING_FIELD
let ghi : Foo.some_other_field = Foo.GHI
