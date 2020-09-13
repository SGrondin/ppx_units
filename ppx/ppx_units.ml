open Ppxlib
open Ast_builder.Default

let deriver = "units"

let unit_attribute =
  Attribute.declare_with_name_loc
    "units.unit"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~name_loc x -> x, name_loc)

let check_at_least_one_record ~loc rec_flag tds =
  begin match rec_flag with
  | Nonrecursive -> Location.raise_errorf ~loc "nonrec is not compatible with the '%s' preprocessor" deriver
  | _ -> ()
  end;
  let is_record = function
  | { ptype_kind = Ptype_record _; _ } -> true
  | _ -> false
  in
  if not (List.exists is_record tds) then
    Location.raise_errorf ~loc
      begin match tds with
      | [_] -> "Unsupported use of '%s' preprocessor (you can only use it on records)."
      | _ -> "'with %s' can only be applied on type definitions in which at least one type definition is a record"
      end deriver

let validate_unit_name ~ty_name ~field_name ~loc unit =
  String.iteri (fun i -> function
  (* https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#capitalized-ident *)
  | 'A' .. 'Z' -> ()
  | 'a' .. 'z' | '0' .. '9' | '_' when i > 0 -> ()
  | c ->
    Location.raise_errorf ~loc "Invalid unit name '%s' in type %s (%s). Invalid character '%c' at index %d."
      unit ty_name field_name c i
  ) unit

let generate_type_def ~ty_name labdec =
  let field_name = labdec.pld_name.txt in
  let unit = begin match Attribute.get unit_attribute labdec with
  | None -> String.uppercase_ascii field_name
  | Some (unit, loc) ->
    validate_unit_name ~ty_name ~field_name ~loc unit;
    unit
  end
  in
  let loc = labdec.pld_loc in

  type_declaration
    ~loc
    ~name:(Located.mk ~loc field_name)
    ~params:[]
    ~cstrs:[]
    ~kind:(Ptype_variant [
        constructor_declaration
          ~loc
          ~name:(Located.mk ~loc unit)
          ~args:(Pcstr_tuple [])
          ~res:None
      ])
    ~private_:Public
    ~manifest:None

let generate ~f ~loc (rec_flag, tds) =
  let tds = List.map name_type_params_in_td tds in
  check_at_least_one_record ~loc rec_flag tds;
  List.concat_map (function
  | { ptype_name = { txt = ty_name; loc }; ptype_kind = Ptype_record labdecs; _ } ->
    List.map (f ~ty_name ~loc) labdecs
  | _ -> []
  ) tds

let generate_sig ~loc ~path:_ =
  generate ~loc ~f:(fun ~ty_name ~loc labdec ->
    psig_type ~loc Recursive [generate_type_def ~ty_name labdec]
  )

let generate_struct ~loc ~path:_ =
  generate ~loc ~f:(fun ~ty_name ~loc labdec ->
    pstr_type ~loc Recursive [generate_type_def ~ty_name labdec]
  )

let units =
  let attributes = [Attribute.T unit_attribute] in
  Deriving.add deriver
    ~sig_type_decl:(Deriving.Generator.make_noarg generate_sig)
    ~str_type_decl:(Deriving.Generator.make_noarg ~attributes generate_struct)
