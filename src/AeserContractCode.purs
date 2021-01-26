module Aeser.Contract.Code(erlps__deserialize__1,
                           erlps__serialize__1, erlps__serialize__2) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.1.0
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
import Erlang.Helpers as H
import Erlang.Exception as EXC
import Erlang.Type (ErlangFun, ErlangTerm(..), weakCmp, weakEq,
                    weakNEq, weakLt, weakLeq, weakGeq, weakGt)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)


erlps__serialize__1 :: ErlangFun
erlps__serialize__1 [codemap_0] =
  (erlps__serialize__2 [codemap_0, (ErlangInt (DBI.fromInt 3))])
erlps__serialize__1 [arg_3] = (EXC.function_clause unit)
erlps__serialize__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__serialize__2 :: ErlangFun
erlps__serialize__2 [codemap_3@(ErlangMap map_0),
                     sophiacontractversion_4]
  | (DM.Just typeinfo_2) <-
      ((Map.lookup (ErlangAtom "type_info") map_0))
  , (DM.Just bytecode_1) <-
      ((Map.lookup (ErlangAtom "byte_code") map_0)) =
  let   
    sourcehash_15 =
      case codemap_3 of
        (ErlangMap map_6) | (DM.Just shash_7) <-
                              ((Map.lookup (ErlangAtom "source_hash") map_6)) ->
          shash_7
        (ErlangMap map_8) | (DM.Just srcstr_9) <-
                              ((Map.lookup (ErlangAtom "contract_source")
                                  map_8)) ->
          let    arg_11 = (BIF.erlang__list_to_binary__1 [srcstr_9])
          in let
            match_expr_14 =
              (BIF.do_remote_fun_call "Enacl" "erlps__generichash__2"
                 [(ErlangInt (DBI.fromInt 32)), arg_11])
          in
            case match_expr_14 of
              (ErlangTuple [(ErlangAtom "ok"), shash_13]) -> shash_13
              _ -> (EXC.badmatch match_expr_14)
        something_else -> (EXC.case_clause something_else)
  in let
    arg_18 =
      (ErlangBinary
         (BIN.concat
            [(BIN.from_int (ErlangInt (DBI.fromInt 117))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.from_int (ErlangInt (DBI.fromInt 110))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.from_int (ErlangInt (DBI.fromInt 107))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.from_int (ErlangInt (DBI.fromInt 110))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.from_int (ErlangInt (DBI.fromInt 111))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.from_int (ErlangInt (DBI.fromInt 119))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.from_int (ErlangInt (DBI.fromInt 110))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big)]))
  in let
    version_26 =
      (BIF.do_remote_fun_call "Maps" "erlps__get__3"
         [(ErlangAtom "compiler_version"), codemap_3, arg_18])
  in let
    binversion_29 =
      case (ErlangAtom "true") of
        _ | (H.isEInt version_26) ->
          (BIF.erlang__integer_to_binary__1 [version_26])
        _ | ((ErlangAtom "true") ==
               (H.falsifyErrors
                  (\ _ -> (BIF.erlang__is_binary__1 [version_26])))) ->
          version_26
        _ -> (EXC.if_clause unit)
  in let
    payable_33 =
      (BIF.do_remote_fun_call "Maps" "erlps__get__3"
         [(ErlangAtom "payable"), codemap_3, (ErlangAtom "true")])
  in let
    head_35 =
      (ErlangTuple [(ErlangAtom "source_hash"), sourcehash_15])
  in let
    head_39 = (ErlangTuple [(ErlangAtom "type_info"), typeinfo_2])
  in let
    head_43 = (ErlangTuple [(ErlangAtom "byte_code"), bytecode_1])
  in let
    cond_49 =
      (BIF.erlang__op_greater
         [sophiacontractversion_4, (ErlangInt (DBI.fromInt 1))])
  in let
    lop_48 =
      case cond_49 of
        (ErlangAtom "true") ->
          let
            lc_ret_52 =
              (ErlangTuple [(ErlangAtom "compiler_version"), binversion_29])
          in (ErlangCons lc_ret_52 ErlangEmptyList)
        _ -> ErlangEmptyList
  in let
    cond_56 =
      (BIF.erlang__op_greater
         [sophiacontractversion_4, (ErlangInt (DBI.fromInt 2))])
  in let
    rop_55 =
      case cond_56 of
        (ErlangAtom "true") ->
          let
            lc_ret_59 = (ErlangTuple [(ErlangAtom "payable"), payable_33])
          in (ErlangCons lc_ret_59 ErlangEmptyList)
        _ -> ErlangEmptyList
  in let rop_47 = (BIF.erlang__op_append [lop_48, rop_55])
  in let
    fields_62 =
      (BIF.erlang__op_append
         [(ErlangCons head_35
             (ErlangCons head_39 (ErlangCons head_43 ErlangEmptyList))),
          rop_47])
  in let
    arg_65 =
      (erlps__serialization_template__1 [sophiacontractversion_4])
  in
    (BIF.do_remote_fun_call "Aeser.Chain.Objects"
       "erlps__serialize__4"
       [(ErlangAtom "compiler_sophia"), sophiacontractversion_4, arg_65,
        fields_62])
erlps__serialize__2 [arg_68, arg_69] = (EXC.function_clause unit)
erlps__serialize__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__deserialize__1 :: ErlangFun
erlps__deserialize__1 [binary_0] =
  let
    case_1 =
      (BIF.do_remote_fun_call "Aeser.Chain.Objects"
         "erlps__deserialize_type_and_vsn__1" [binary_0])
  in
    case case_1 of
      (ErlangTuple [type_3@(ErlangAtom "compiler_sophia"),
                    vsn_5@(ErlangInt num_4), _rest_6]) | ((ErlangInt num_4) ==
                                                            (ErlangInt
                                                               (DBI.fromInt
                                                                  1))) ->
        let    template_8 = (erlps__serialization_template__1 [vsn_5])
        in let
          match_expr_16 =
            (BIF.do_remote_fun_call "Aeser.Chain.Objects"
               "erlps__deserialize__4" [type_3, vsn_5, template_8, binary_0])
        in
          case match_expr_16 of
            (ErlangCons (ErlangTuple [(ErlangAtom "source_hash"),
                                      hash_13]) (ErlangCons (ErlangTuple [(ErlangAtom "type_info"),
                                                                          typeinfo_14]) (ErlangCons (ErlangTuple [(ErlangAtom "byte_code"),
                                                                                                                  bytecode_15]) (ErlangEmptyList)))) ->
              (ErlangMap
                 (Map.fromFoldable
                    [(Tup.Tuple (ErlangAtom "source_hash") hash_13),
                     (Tup.Tuple (ErlangAtom "type_info") typeinfo_14),
                     (Tup.Tuple (ErlangAtom "byte_code") bytecode_15),
                     (Tup.Tuple (ErlangAtom "contract_vsn") vsn_5),
                     (Tup.Tuple (ErlangAtom "payable") (ErlangAtom "true"))]))
            _ -> (EXC.badmatch match_expr_16)
      (ErlangTuple [type_27@(ErlangAtom "compiler_sophia"),
                    vsn_29@(ErlangInt num_28), _rest_30]) | ((ErlangInt
                                                                num_28) ==
                                                               (ErlangInt
                                                                  (DBI.fromInt
                                                                     2))) ->
        let    template_32 = (erlps__serialization_template__1 [vsn_29])
        in let
          match_expr_41 =
            (BIF.do_remote_fun_call "Aeser.Chain.Objects"
               "erlps__deserialize__4" [type_27, vsn_29, template_32, binary_0])
        in
          case match_expr_41 of
            (ErlangCons (ErlangTuple [(ErlangAtom "source_hash"),
                                      hash_37]) (ErlangCons (ErlangTuple [(ErlangAtom "type_info"),
                                                                          typeinfo_38]) (ErlangCons (ErlangTuple [(ErlangAtom "byte_code"),
                                                                                                                  bytecode_39]) (ErlangCons (ErlangTuple [(ErlangAtom "compiler_version"),
                                                                                                                                                          compilerversion_40]) (ErlangEmptyList))))) ->
              (ErlangMap
                 (Map.fromFoldable
                    [(Tup.Tuple (ErlangAtom "source_hash") hash_37),
                     (Tup.Tuple (ErlangAtom "type_info") typeinfo_38),
                     (Tup.Tuple (ErlangAtom "byte_code") bytecode_39),
                     (Tup.Tuple (ErlangAtom "compiler_version")
                        compilerversion_40),
                     (Tup.Tuple (ErlangAtom "contract_vsn") vsn_29),
                     (Tup.Tuple (ErlangAtom "payable") (ErlangAtom "true"))]))
            _ -> (EXC.badmatch match_expr_41)
      (ErlangTuple [type_54@(ErlangAtom "compiler_sophia"),
                    vsn_56@(ErlangInt num_55), _rest_57]) | ((ErlangInt
                                                                num_55) ==
                                                               (ErlangInt
                                                                  (DBI.fromInt
                                                                     3))) ->
        let    template_59 = (erlps__serialization_template__1 [vsn_56])
        in let
          match_expr_69 =
            (BIF.do_remote_fun_call "Aeser.Chain.Objects"
               "erlps__deserialize__4" [type_54, vsn_56, template_59, binary_0])
        in
          case match_expr_69 of
            (ErlangCons (ErlangTuple [(ErlangAtom "source_hash"),
                                      hash_64]) (ErlangCons (ErlangTuple [(ErlangAtom "type_info"),
                                                                          typeinfo_65]) (ErlangCons (ErlangTuple [(ErlangAtom "byte_code"),
                                                                                                                  bytecode_66]) (ErlangCons (ErlangTuple [(ErlangAtom "compiler_version"),
                                                                                                                                                          compilerversion_67]) (ErlangCons (ErlangTuple [(ErlangAtom "payable"),
                                                                                                                                                                                                         payable_68]) (ErlangEmptyList)))))) ->
              (ErlangMap
                 (Map.fromFoldable
                    [(Tup.Tuple (ErlangAtom "source_hash") hash_64),
                     (Tup.Tuple (ErlangAtom "type_info") typeinfo_65),
                     (Tup.Tuple (ErlangAtom "byte_code") bytecode_66),
                     (Tup.Tuple (ErlangAtom "compiler_version")
                        compilerversion_67),
                     (Tup.Tuple (ErlangAtom "contract_vsn") vsn_56),
                     (Tup.Tuple (ErlangAtom "payable") payable_68)]))
            _ -> (EXC.badmatch match_expr_69)
      other_82 ->
        let
          arg_83 =
            (ErlangTuple [(ErlangAtom "illegal_code_object"), other_82])
        in (BIF.erlang__error__1 [arg_83])
erlps__deserialize__1 [arg_86] = (EXC.function_clause unit)
erlps__deserialize__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__serialization_template__1 :: ErlangFun
erlps__serialization_template__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 1))) =
  let   
    head_1 =
      (ErlangTuple [(ErlangAtom "source_hash"), (ErlangAtom "binary")])
  in let
    head_8 =
      (ErlangTuple
         [(ErlangAtom "binary"), (ErlangAtom "binary"),
          (ErlangAtom "binary"), (ErlangAtom "binary")])
  in let
    head_5 =
      (ErlangTuple
         [(ErlangAtom "type_info"), (ErlangCons head_8 ErlangEmptyList)])
  in let
    head_15 =
      (ErlangTuple [(ErlangAtom "byte_code"), (ErlangAtom "binary")])
  in
    (ErlangCons head_1
       (ErlangCons head_5 (ErlangCons head_15 ErlangEmptyList)))
erlps__serialization_template__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 2))) =
  let   
    head_1 =
      (ErlangTuple [(ErlangAtom "source_hash"), (ErlangAtom "binary")])
  in let
    head_8 =
      (ErlangTuple
         [(ErlangAtom "binary"), (ErlangAtom "binary"),
          (ErlangAtom "binary"), (ErlangAtom "binary")])
  in let
    head_5 =
      (ErlangTuple
         [(ErlangAtom "type_info"), (ErlangCons head_8 ErlangEmptyList)])
  in let
    head_15 =
      (ErlangTuple [(ErlangAtom "byte_code"), (ErlangAtom "binary")])
  in let
    head_19 =
      (ErlangTuple
         [(ErlangAtom "compiler_version"), (ErlangAtom "binary")])
  in
    (ErlangCons head_1
       (ErlangCons head_5
          (ErlangCons head_15 (ErlangCons head_19 ErlangEmptyList))))
erlps__serialization_template__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 3))) =
  let   
    head_1 =
      (ErlangTuple [(ErlangAtom "source_hash"), (ErlangAtom "binary")])
  in let
    head_8 =
      (ErlangTuple
         [(ErlangAtom "binary"), (ErlangAtom "binary"),
          (ErlangAtom "bool"), (ErlangAtom "binary"),
          (ErlangAtom "binary")])
  in let
    head_5 =
      (ErlangTuple
         [(ErlangAtom "type_info"), (ErlangCons head_8 ErlangEmptyList)])
  in let
    head_16 =
      (ErlangTuple [(ErlangAtom "byte_code"), (ErlangAtom "binary")])
  in let
    head_20 =
      (ErlangTuple
         [(ErlangAtom "compiler_version"), (ErlangAtom "binary")])
  in let
    head_24 =
      (ErlangTuple [(ErlangAtom "payable"), (ErlangAtom "bool")])
  in
    (ErlangCons head_1
       (ErlangCons head_5
          (ErlangCons head_16
             (ErlangCons head_20 (ErlangCons head_24 ErlangEmptyList)))))
erlps__serialization_template__1 [arg_28] =
  (EXC.function_clause unit)
erlps__serialization_template__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)