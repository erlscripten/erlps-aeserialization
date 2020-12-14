module Erl.Bits(erlps__system_bittypes__0,
                erlps__system_bitdefault__0, erlps__set_bit_type__2,
                erlps__as_list__1) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.0.2
-}

import Prelude
import Data.Array as DA
import Data.List as DL
import Data.Maybe as DM
import Data.Map as Map
import Data.Tuple as Tup
import Data.BigInt as DBI
import Erlang.Builtins as BIF
import Erlang.Binary as BIN
import Erlang.Helpers
import Erlang.Exception as EXC
import Erlang.Type (ErlangFun, ErlangTerm(..), weakCmp, weakEq,
                    weakNEq, weakLt, weakLeq, weakGeq, weakGt)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)


erlps__system_bitdefault__0 :: ErlangFun
erlps__system_bitdefault__0 [] =
  (ErlangAtom "no_system_bitdefault")
erlps__system_bitdefault__0 args =
  (EXC.badarity
     (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__system_bittypes__0 :: ErlangFun
erlps__system_bittypes__0 [] = (ErlangAtom "no_system_types")
erlps__system_bittypes__0 args =
  (EXC.badarity
     (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__as_list__1 :: ErlangFun
erlps__as_list__1 [bt_0] =
  let   
    head_1 =
      case bt_0 of
        (ErlangTuple arr_4) | (DM.Just field_3) <- ((arr_4 DA.!! 1)) ->
          field_3
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    tup_el_8 =
      case bt_0 of
        (ErlangTuple arr_11) | (DM.Just field_10) <-
                                 ((arr_11 DA.!! 2)) ->
          field_10
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let head_6 = (ErlangTuple [(ErlangAtom "unit"), tup_el_8])
  in let
    head_13 =
      case bt_0 of
        (ErlangTuple arr_16) | (DM.Just field_15) <-
                                 ((arr_16 DA.!! 3)) ->
          field_15
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    head_18 =
      case bt_0 of
        (ErlangTuple arr_21) | (DM.Just field_20) <-
                                 ((arr_21 DA.!! 4)) ->
          field_20
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in
    (ErlangCons head_1
       (ErlangCons head_6
          (ErlangCons head_13 (ErlangCons head_18 ErlangEmptyList))))
erlps__as_list__1 [arg_23] = (EXC.function_clause unit)
erlps__as_list__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__set_bit_type__2 :: ErlangFun
erlps__set_bit_type__2 [size_0, (ErlangAtom "default")] =
  (erlps__set_bit_type__2 [size_0, ErlangEmptyList])
erlps__set_bit_type__2 [size_0, typelist_1] =
  (EXC.tryCatch
     (\ _ ->
        let match_expr_7 = (erlps__set_bit__1 [typelist_1])
        in
          case match_expr_7 of
            (ErlangTuple [(ErlangAtom "bittype"), type_3, unit_4, sign_5,
                          endian_6]) ->
              (erlps__apply_defaults__5
                 [type_3, size_0, unit_4, sign_5, endian_6])
            _ -> (EXC.badmatch match_expr_7))
     (\ ex_14 ->
        case ex_14 of
          (ErlangTuple [(ErlangAtom "throw"), error_15, _]) -> error_15
          ex_14 -> (EXC.raise ex_14)))
erlps__set_bit_type__2 [arg_16, arg_17] =
  (EXC.function_clause unit)
erlps__set_bit_type__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__set_bit__1 :: ErlangFun
erlps__set_bit__1 [(ErlangEmptyList)] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__set_bit__1 [(ErlangCons h_0 t_1)] =
  let arg_3 = (erlps__type_to_record__1 [h_0])
  in (erlps__set_bit_1__2 [t_1, arg_3])
erlps__set_bit__1 [arg_5] = (EXC.function_clause unit)
erlps__set_bit__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__set_bit_1__2 :: ErlangFun
erlps__set_bit_1__2 [(ErlangCons t0_0 ts_1), bt0_2] =
  let    type_4 = (erlps__type_to_record__1 [t0_0])
  in let bt_7 = (erlps__merge_bittype__2 [type_4, bt0_2])
  in (erlps__set_bit_1__2 [ts_1, bt_7])
erlps__set_bit_1__2 [(ErlangEmptyList), bt_0] = bt_0
erlps__set_bit_1__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__set_bit_1__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__type_to_record__1 :: ErlangFun
erlps__type_to_record__1 [(ErlangAtom "integer")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "integer"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "utf8")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "utf8"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "utf16")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "utf16"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "utf32")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "utf32"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "float")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "float"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "binary")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "binary"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "bytes")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "binary"),
      (ErlangInt (DBI.fromInt 8)), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "bitstring")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "binary"),
      (ErlangInt (DBI.fromInt 1)), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "bits")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "binary"),
      (ErlangInt (DBI.fromInt 1)), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangTuple [(ErlangAtom "unit"),
                                        (ErlangAtom "undefined")])]
  =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangTuple [(ErlangAtom "unit"),
                                        sz_0])]
  | (((isEInt sz_0) &&
        (weakGt sz_0 (ErlangInt (DBI.fromInt 0)))) &&
       (weakLeq sz_0 (ErlangInt (DBI.fromInt 256)))) =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"), sz_0,
      (ErlangAtom "undefined"), (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "big")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "big")])
erlps__type_to_record__1 [(ErlangAtom "little")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "little")])
erlps__type_to_record__1 [(ErlangAtom "native")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "undefined"),
      (ErlangAtom "native")])
erlps__type_to_record__1 [(ErlangAtom "signed")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "signed"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [(ErlangAtom "unsigned")] =
  (ErlangTuple
     [(ErlangAtom "bittype"), (ErlangAtom "undefined"),
      (ErlangAtom "undefined"), (ErlangAtom "unsigned"),
      (ErlangAtom "undefined")])
erlps__type_to_record__1 [name_0] =
  let   
    tup_el_3 =
      (ErlangTuple [(ErlangAtom "undefined_bittype"), name_0])
  in let arg_1 = (ErlangTuple [(ErlangAtom "error"), tup_el_3])
  in (BIF.erlang__throw__1 [arg_1])
erlps__type_to_record__1 [arg_6] = (EXC.function_clause unit)
erlps__type_to_record__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__merge_bittype__2 :: ErlangFun
erlps__merge_bittype__2 [b1_0, b2_1] =
  let   
    arg_2 =
      case b1_0 of
        (ErlangTuple arr_5) | (DM.Just field_4) <- ((arr_5 DA.!! 4)) ->
          field_4
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    arg_6 =
      case b2_1 of
        (ErlangTuple arr_9) | (DM.Just field_8) <- ((arr_9 DA.!! 4)) ->
          field_8
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    endian_11 =
      (erlps__merge_field__3 [arg_2, arg_6, (ErlangAtom "endianness")])
  in let
    arg_12 =
      case b1_0 of
        (ErlangTuple arr_15) | (DM.Just field_14) <-
                                 ((arr_15 DA.!! 3)) ->
          field_14
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    arg_16 =
      case b2_1 of
        (ErlangTuple arr_19) | (DM.Just field_18) <-
                                 ((arr_19 DA.!! 3)) ->
          field_18
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    sign_21 =
      (erlps__merge_field__3 [arg_12, arg_16, (ErlangAtom "sign")])
  in let
    arg_22 =
      case b1_0 of
        (ErlangTuple arr_25) | (DM.Just field_24) <-
                                 ((arr_25 DA.!! 1)) ->
          field_24
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    arg_26 =
      case b2_1 of
        (ErlangTuple arr_29) | (DM.Just field_28) <-
                                 ((arr_29 DA.!! 1)) ->
          field_28
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    type_31 =
      (erlps__merge_field__3 [arg_22, arg_26, (ErlangAtom "type")])
  in let
    arg_32 =
      case b1_0 of
        (ErlangTuple arr_35) | (DM.Just field_34) <-
                                 ((arr_35 DA.!! 2)) ->
          field_34
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    arg_36 =
      case b2_1 of
        (ErlangTuple arr_39) | (DM.Just field_38) <-
                                 ((arr_39 DA.!! 2)) ->
          field_38
        _ -> (EXC.badrecord (ErlangAtom "bittype"))
  in let
    unit_41 =
      (erlps__merge_field__3 [arg_32, arg_36, (ErlangAtom "unit")])
  in
    (ErlangTuple
       [(ErlangAtom "bittype"), type_31, unit_41, sign_21, endian_11])
erlps__merge_bittype__2 [arg_47, arg_48] =
  (EXC.function_clause unit)
erlps__merge_bittype__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__merge_field__3 :: ErlangFun
erlps__merge_field__3 [(ErlangAtom "undefined"), b_0, _] = b_0
erlps__merge_field__3 [a_0, (ErlangAtom "undefined"), _] = a_0
erlps__merge_field__3 [a_0, a_1, _] | (a_1 == a_0) = a_0
erlps__merge_field__3 [x_0, y_1, what_2] =
  let    tup_el_9 = (BIF.erlang__atom_to_list__1 [what_2])
  in let
    tup_el_5 =
      (ErlangTuple
         [(ErlangAtom "bittype_mismatch"), x_0, y_1, tup_el_9])
  in let arg_3 = (ErlangTuple [(ErlangAtom "error"), tup_el_5])
  in (BIF.erlang__throw__1 [arg_3])
erlps__merge_field__3 [arg_11, arg_12, arg_13] =
  (EXC.function_clause unit)
erlps__merge_field__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__apply_defaults__5 :: ErlangFun
erlps__apply_defaults__5 [(ErlangAtom "undefined"), size_0,
                          unit_1, sign_2, endian_3]
  =
  (erlps__apply_defaults__5
     [(ErlangAtom "integer"), size_0, unit_1, sign_2, endian_3])
erlps__apply_defaults__5 [(ErlangAtom "binary"),
                          (ErlangAtom "default"), unit_0, sign_1, endian_2]
  =
  (erlps__apply_defaults__5
     [(ErlangAtom "binary"), (ErlangAtom "all"), unit_0, sign_1,
      endian_2])
erlps__apply_defaults__5 [(ErlangAtom "integer"),
                          (ErlangAtom "default"), unit_0, sign_1, endian_2]
  =
  let _ = (erlps__check_unit__1 [unit_0])
  in
    (erlps__apply_defaults__5
       [(ErlangAtom "integer"), (ErlangInt (DBI.fromInt 8)),
        (ErlangInt (DBI.fromInt 1)), sign_1, endian_2])
erlps__apply_defaults__5 [type_0@(ErlangAtom "utf8"),
                          (ErlangAtom "default"), unit_1, sign_2, endian_3]
  =
  (erlps__apply_defaults__5
     [type_0, (ErlangAtom "undefined"), unit_1, sign_2, endian_3])
erlps__apply_defaults__5 [type_0@(ErlangAtom "utf16"),
                          (ErlangAtom "default"), unit_1, sign_2, endian_3]
  =
  (erlps__apply_defaults__5
     [type_0, (ErlangAtom "undefined"), unit_1, sign_2, endian_3])
erlps__apply_defaults__5 [type_0@(ErlangAtom "utf32"),
                          (ErlangAtom "default"), unit_1, sign_2, endian_3]
  =
  (erlps__apply_defaults__5
     [type_0, (ErlangAtom "undefined"), unit_1, sign_2, endian_3])
erlps__apply_defaults__5 [(ErlangAtom "float"),
                          (ErlangAtom "default"), unit_0, sign_1, endian_2]
  =
  let _ = (erlps__check_unit__1 [unit_0])
  in
    (erlps__apply_defaults__5
       [(ErlangAtom "float"), (ErlangInt (DBI.fromInt 64)),
        (ErlangInt (DBI.fromInt 1)), sign_1, endian_2])
erlps__apply_defaults__5 [(ErlangAtom "binary"), size_0,
                          (ErlangAtom "undefined"), sign_1, endian_2]
  =
  (erlps__apply_defaults__5
     [(ErlangAtom "binary"), size_0, (ErlangInt (DBI.fromInt 8)),
      sign_1, endian_2])
erlps__apply_defaults__5 [(ErlangAtom "integer"), size_0,
                          (ErlangAtom "undefined"), sign_1, endian_2]
  =
  (erlps__apply_defaults__5
     [(ErlangAtom "integer"), size_0, (ErlangInt (DBI.fromInt 1)),
      sign_1, endian_2])
erlps__apply_defaults__5 [(ErlangAtom "float"), size_0,
                          (ErlangAtom "undefined"), sign_1, endian_2]
  =
  (erlps__apply_defaults__5
     [(ErlangAtom "float"), size_0, (ErlangInt (DBI.fromInt 1)),
      sign_1, endian_2])
erlps__apply_defaults__5 [type_0, size_1, unit_2,
                          (ErlangAtom "undefined"), endian_3]
  =
  (erlps__apply_defaults__5
     [type_0, size_1, unit_2, (ErlangAtom "unsigned"), endian_3])
erlps__apply_defaults__5 [type_0, size_1, unit_2, sign_3,
                          (ErlangAtom "undefined")]
  =
  (erlps__apply_defaults__5
     [type_0, size_1, unit_2, sign_3, (ErlangAtom "big")])
erlps__apply_defaults__5 [type_0, size_1, unit_2, sign_3,
                          endian_4]
  =
  let    _ = (erlps__check_size_unit__3 [type_0, size_1, unit_2])
  in let
    tup_el_10 =
      (ErlangTuple
         [(ErlangAtom "bittype"), type_0, unit_2, sign_3, endian_4])
  in (ErlangTuple [(ErlangAtom "ok"), size_1, tup_el_10])
erlps__apply_defaults__5 [arg_16, arg_17, arg_18, arg_19, arg_20]
  =
  (EXC.function_clause unit)
erlps__apply_defaults__5 args =
  (EXC.badarity
     (ErlangFun 5 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__check_size_unit__3 :: ErlangFun
erlps__check_size_unit__3 [(ErlangAtom "utf8"), size_0, unit_1] =
  (erlps__check_size_unit_1__2 [size_0, unit_1])
erlps__check_size_unit__3 [(ErlangAtom "utf16"), size_0, unit_1]
  =
  (erlps__check_size_unit_1__2 [size_0, unit_1])
erlps__check_size_unit__3 [(ErlangAtom "utf32"), size_0, unit_1]
  =
  (erlps__check_size_unit_1__2 [size_0, unit_1])
erlps__check_size_unit__3 [_, _, _] = (ErlangAtom "ok")
erlps__check_size_unit__3 [arg_0, arg_1, arg_2] =
  (EXC.function_clause unit)
erlps__check_size_unit__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__check_size_unit_1__2 :: ErlangFun
erlps__check_size_unit_1__2 [size_0, unit_1] =
  let
    _ =
      case size_0 of
        (ErlangAtom "default") -> (ErlangAtom "ok")
        (ErlangAtom "undefined") -> (ErlangAtom "ok")
        (ErlangTuple [(ErlangAtom "atom"), _,
                      (ErlangAtom "undefined")]) ->
          (ErlangAtom "ok")
        (ErlangTuple [(ErlangAtom "value"), _,
                      (ErlangAtom "undefined")]) ->
          (ErlangAtom "ok")
        _ ->
          let
            arg_3 =
              (ErlangTuple
                 [(ErlangAtom "error"),
                  (ErlangAtom "utf_bittype_size_or_unit")])
          in (BIF.erlang__throw__1 [arg_3])
        something_else -> (EXC.case_clause something_else)
  in
    case unit_1 of
      (ErlangAtom "undefined") -> (ErlangAtom "ok")
      _ ->
        let
          arg_7 =
            (ErlangTuple
               [(ErlangAtom "error"), (ErlangAtom "utf_bittype_size_or_unit")])
        in (BIF.erlang__throw__1 [arg_7])
      something_else -> (EXC.case_clause something_else)
erlps__check_size_unit_1__2 [arg_10, arg_11] =
  (EXC.function_clause unit)
erlps__check_size_unit_1__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__check_unit__1 :: ErlangFun
erlps__check_unit__1 [(ErlangAtom "undefined")] =
  (ErlangAtom "ok")
erlps__check_unit__1 [_] =
  let
    arg_0 =
      (ErlangTuple [(ErlangAtom "error"), (ErlangAtom "bittype_unit")])
  in (BIF.erlang__throw__1 [arg_0])
erlps__check_unit__1 [arg_3] = (EXC.function_clause unit)
erlps__check_unit__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)