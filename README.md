# ppx_units

ppx_units is a simple deriver to automatically generate single variant types from record type definitions.

```ocaml
type person = {
  id: string;
  first_name: int [@unit "FN"];
} [@@deriving units]
```
will generate
```ocaml
type id = ID

type first_name = FN
```
