module Foo : sig
  type t = {
    some_int_field : int [@unit "ABC"];
    some_string_field : string;
    some_other_field : string [@units.unit "GHI"];
  }
  [@@deriving units]
end

val abc: Foo.some_int_field

val def: Foo.some_string_field

val ghi: Foo.some_other_field
