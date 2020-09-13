module Foo = struct
  type t = {
    some_int_field : int [@unit "ABC"];
    some_string_field : string;
    some_other_field : string [@units.unit "GHI"];
  }
  [@@deriving units]
end

let abc : Foo.some_int_field = ABC

let def : Foo.some_string_field = Foo.SOME_STRING_FIELD

let ghi : Foo.some_other_field = Foo.GHI
