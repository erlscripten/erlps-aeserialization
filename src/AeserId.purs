module Aeser.Id(erlps__create__2, erlps__specialize__1,
                erlps__specialize__2, erlps__specialize_type__1,
                erlps__is_id__1, erlps__encode__1, erlps__decode__1) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.2.0
-}

import Prelude
import Data.BigInt as DBI
import Data.Array as DA
import Data.Maybe as DM
import Data.Map as Map
import Data.Tuple as DT
import Erlang.Builtins as BIF
import Erlang.Binary as BIN
import Erlang.Helpers
import Erlang.Exception as EXC
import Erlang.Type
import Partial.Unsafe (unsafePartial)


erlps__create__2 :: ErlangFun
erlps__create__2 [tag_0, val_1]
  | ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let   
               lop_9 = BIF.erlang__op_exactEq [tag_0, ErlangAtom "account"]
             in let
               lop_8 =
                 case lop_9 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "oracle"]
                   _ -> EXC.badarg1 lop_9
             in let
               lop_7 =
                 case lop_8 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "name"]
                   _ -> EXC.badarg1 lop_8
             in let
               lop_6 =
                 case lop_7 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "commitment"]
                   _ -> EXC.badarg1 lop_7
             in let
               lop_5 =
                 case lop_6 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "contract"]
                   _ -> EXC.badarg1 lop_6
             in
               case lop_5 of
                 (ErlangAtom "true") -> ErlangAtom "true"
                 (ErlangAtom "false") ->
                   let
                     lop_20 =
                       BIF.erlang__op_exactEq [tag_0, ErlangAtom "channel"]
                   in
                     case lop_20 of
                       (ErlangAtom "false") -> ErlangAtom "false"
                       (ErlangAtom "true") ->
                         let    lop_23 = BIF.erlang__byte_size__1 [val_1]
                         in let rop_25 = toErl 32
                         in BIF.erlang__op_exactEq [lop_23, rop_25]
                       _ -> EXC.badarg1 lop_20
                 _ -> EXC.badarg1 lop_5))) =
  ErlangTuple [ErlangAtom "id", tag_0, val_1]
erlps__create__2 [tag_0, val_1]
  | ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let    lop_5 = BIF.erlang__byte_size__1 [val_1]
             in let rop_7 = toErl 32
             in BIF.erlang__op_exactEq [lop_5, rop_7]))) =
  let arg_2 = ErlangTuple [ErlangAtom "illegal_tag", tag_0]
  in BIF.erlang__error__1 [arg_2]
erlps__create__2 [tag_0, val_1]
  | ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let   
               lop_9 = BIF.erlang__op_exactEq [tag_0, ErlangAtom "account"]
             in let
               lop_8 =
                 case lop_9 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "oracle"]
                   _ -> EXC.badarg1 lop_9
             in let
               lop_7 =
                 case lop_8 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "name"]
                   _ -> EXC.badarg1 lop_8
             in let
               lop_6 =
                 case lop_7 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "commitment"]
                   _ -> EXC.badarg1 lop_7
             in let
               lop_5 =
                 case lop_6 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "contract"]
                   _ -> EXC.badarg1 lop_6
             in
               case lop_5 of
                 (ErlangAtom "true") -> ErlangAtom "true"
                 (ErlangAtom "false") ->
                   BIF.erlang__op_exactEq [tag_0, ErlangAtom "channel"]
                 _ -> EXC.badarg1 lop_5))) =
  let arg_2 = ErlangTuple [ErlangAtom "illegal_val", val_1]
  in BIF.erlang__error__1 [arg_2]
erlps__create__2 [tag_0, val_1] =
  let
    arg_2 =
      ErlangTuple [ErlangAtom "illegal_tag_and_val", tag_0, val_1]
  in BIF.erlang__error__1 [arg_2]
erlps__create__2 [arg_6, arg_7] = EXC.function_clause unit
erlps__create__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__specialize__1 :: ErlangFun
erlps__specialize__1 [(ErlangTuple [(ErlangAtom "id"), tag_0,
                                    val_1])]
  =
  ErlangTuple [tag_0, val_1]
erlps__specialize__1 [arg_4] = EXC.function_clause unit
erlps__specialize__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__specialize__2 :: ErlangFun
erlps__specialize__2 [(ErlangTuple [(ErlangAtom "id"), tag_0,
                                    val_1]),
                      tag_2]
  | (tag_2 == tag_0)
  , ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let   
               lop_7 = BIF.erlang__op_exactEq [tag_0, ErlangAtom "account"]
             in let
               lop_6 =
                 case lop_7 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "oracle"]
                   _ -> EXC.badarg1 lop_7
             in let
               lop_5 =
                 case lop_6 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "name"]
                   _ -> EXC.badarg1 lop_6
             in let
               lop_4 =
                 case lop_5 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "commitment"]
                   _ -> EXC.badarg1 lop_5
             in let
               lop_3 =
                 case lop_4 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "contract"]
                   _ -> EXC.badarg1 lop_4
             in
               case lop_3 of
                 (ErlangAtom "true") -> ErlangAtom "true"
                 (ErlangAtom "false") ->
                   let
                     lop_18 =
                       BIF.erlang__op_exactEq [tag_0, ErlangAtom "channel"]
                   in
                     case lop_18 of
                       (ErlangAtom "false") -> ErlangAtom "false"
                       (ErlangAtom "true") ->
                         let    lop_21 = BIF.erlang__byte_size__1 [val_1]
                         in let rop_23 = toErl 32
                         in BIF.erlang__op_exactEq [lop_21, rop_23]
                       _ -> EXC.badarg1 lop_18
                 _ -> EXC.badarg1 lop_3))) =
  val_1
erlps__specialize__2 [arg_24, arg_25] = EXC.function_clause unit
erlps__specialize__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__specialize_type__1 :: ErlangFun
erlps__specialize_type__1 [(ErlangTuple [(ErlangAtom "id"),
                                         tag_0, _])]
  | ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let   
               lop_5 = BIF.erlang__op_exactEq [tag_0, ErlangAtom "account"]
             in let
               lop_4 =
                 case lop_5 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "oracle"]
                   _ -> EXC.badarg1 lop_5
             in let
               lop_3 =
                 case lop_4 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "name"]
                   _ -> EXC.badarg1 lop_4
             in let
               lop_2 =
                 case lop_3 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "commitment"]
                   _ -> EXC.badarg1 lop_3
             in let
               lop_1 =
                 case lop_2 of
                   (ErlangAtom "true") -> ErlangAtom "true"
                   (ErlangAtom "false") ->
                     BIF.erlang__op_exactEq [tag_0, ErlangAtom "contract"]
                   _ -> EXC.badarg1 lop_2
             in
               case lop_1 of
                 (ErlangAtom "true") -> ErlangAtom "true"
                 (ErlangAtom "false") ->
                   BIF.erlang__op_exactEq [tag_0, ErlangAtom "channel"]
                 _ -> EXC.badarg1 lop_1))) =
  tag_0
erlps__specialize_type__1 [arg_18] = EXC.function_clause unit
erlps__specialize_type__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__is_id__1 :: ErlangFun
erlps__is_id__1 [(ErlangTuple [(ErlangAtom "id"), _, _])] =
  ErlangAtom "true"
erlps__is_id__1 [_] = ErlangAtom "false"
erlps__is_id__1 [arg_0] = EXC.function_clause unit
erlps__is_id__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__encode__1 :: ErlangFun
erlps__encode__1 [(ErlangTuple [(ErlangAtom "id"), tag_0,
                                val_1])]
  =
  let    bin_el_2 = erlps__encode_tag__1 [tag_0]
  in let
    res_5 =
      ErlangBinary
        (BIN.concat
           [BIN.fromInt bin_el_2 (toErl 1) 8 BIN.Big,
            BIN.binPrefix val_1 (BIN.packedSize val_1) 8])
  in let lop_6 = toErl 33
  in let rop_7 = BIF.erlang__byte_size__1 [res_5]
  in let matchExpr_9 = BIF.erlang__op_exactEq [lop_6, rop_7]
  in
    case matchExpr_9 of
      (ErlangAtom "true") -> res_5
      _ -> EXC.badmatch matchExpr_9
erlps__encode__1 [arg_10] = EXC.function_clause unit
erlps__encode__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__decode__1 :: ErlangFun
erlps__decode__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 1)
  , (BIN.Ok tag_3 bin_2) <-
      (BIN.chopInt binSeg_0 size_1 8 BIN.Big BIN.Unsigned)
  , (ErlangInt size_4) <- (toErl 32)
  , (BIN.Ok val_6 bin_5) <- (BIN.chopBin bin_2 size_4 8)
  , BIN.empty bin_5 =
  let tup_el_8 = erlps__decode_tag__1 [tag_3]
  in ErlangTuple [ErlangAtom "id", tup_el_8, val_6]
erlps__decode__1 [arg_11] = EXC.function_clause unit
erlps__decode__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__encode_tag__1 :: ErlangFun
erlps__encode_tag__1 [(ErlangAtom "account")] = toErl 1
erlps__encode_tag__1 [(ErlangAtom "name")] = toErl 2
erlps__encode_tag__1 [(ErlangAtom "commitment")] = toErl 3
erlps__encode_tag__1 [(ErlangAtom "oracle")] = toErl 4
erlps__encode_tag__1 [(ErlangAtom "contract")] = toErl 5
erlps__encode_tag__1 [(ErlangAtom "channel")] = toErl 6
erlps__encode_tag__1 [other_0] =
  let
    arg_1 = ErlangTuple [ErlangAtom "illegal_id_tag_name", other_0]
  in BIF.erlang__error__1 [arg_1]
erlps__encode_tag__1 [arg_4] = EXC.function_clause unit
erlps__encode_tag__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__decode_tag__1 :: ErlangFun
erlps__decode_tag__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (toErl 1)) =
  ErlangAtom "account"
erlps__decode_tag__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (toErl 2)) =
  ErlangAtom "name"
erlps__decode_tag__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (toErl 3)) =
  ErlangAtom "commitment"
erlps__decode_tag__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (toErl 4)) =
  ErlangAtom "oracle"
erlps__decode_tag__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (toErl 5)) =
  ErlangAtom "contract"
erlps__decode_tag__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (toErl 6)) =
  ErlangAtom "channel"
erlps__decode_tag__1 [x_0] =
  let arg_1 = ErlangTuple [ErlangAtom "illegal_id_tag", x_0]
  in BIF.erlang__error__1 [arg_1]
erlps__decode_tag__1 [arg_4] = EXC.function_clause unit
erlps__decode_tag__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args