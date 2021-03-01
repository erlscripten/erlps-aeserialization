module Aeser.Rlp(erlps__decode__1, erlps__decode_one__1,
                 erlps__encode__1) where
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


erlps__encode__1 :: ErlangFun
erlps__encode__1 [x_0] = erlps__encode__2 [x_0, ErlangEmptyList]
erlps__encode__1 [arg_3] = EXC.function_clause unit
erlps__encode__1 args =
  EXC.badarity (ErlangFun 1 erlps__encode__1) args

erlps__encode__2 :: ErlangFun
erlps__encode__2 [x_4@(ErlangBinary binSeg_0), _opts_5]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok b_3 bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , BIN.empty bin_2
  , weakLeq b_3 (toErl 127) =
  x_4
erlps__encode__2 [x_0, _opts_1] | isEBinary x_0 =
  let arg_2 = toErl 128
  in erlps__add_size__2 [arg_2, x_0]
erlps__encode__2 [l_0, opts_1] | isEList l_0 =
  let   
    bytearray_9 =
      BIN.concatErl
        (flmap
           (\ lc_4 ->
              let    bin_el_6 = erlps__encode__2 [lc_4, opts_1]
              in let
                lcRet_5 =
                  ErlangBinary
                    (BIN.binPrefix bin_el_6 (BIN.packedSize bin_el_6) 8)
              in ErlangCons lcRet_5 ErlangEmptyList)
           l_0)
  in let arg_10 = toErl 192
  in erlps__add_size__2 [arg_10, bytearray_9]
erlps__encode__2 [arg_12, arg_13] = EXC.function_clause unit
erlps__encode__2 args =
  EXC.badarity (ErlangFun 2 erlps__encode__2) args

erlps__add_size__2 :: ErlangFun
erlps__add_size__2 [offset_0, x_1]
  | (ErlangAtom "true") ==
      (falsifyErrors
         (\ _ ->
            let    lop_7 = BIF.erlang__byte_size__1 [x_1]
            in let rop_9 = toErl 55
            in BIF.erlang__op_lesserEq [lop_7, rop_9])) =
  let    rop_4 = BIF.erlang__byte_size__1 [x_1]
  in let bin_el_2 = BIF.erlang__op_plus [offset_0, rop_4]
  in
    ErlangBinary
      (BIN.concat
         [BIN.fromInt bin_el_2 (toErl 8) 1 BIN.Big,
          BIN.binPrefix x_1 (BIN.packedSize x_1) 8])
erlps__add_size__2 [offset_0, x_1] | isEBinary x_1 =
  let    arg_2 = BIF.erlang__byte_size__1 [x_1]
  in let sizebin_4 = BIF.binary__encode_unsigned__1 [arg_2]
  in let lop_6 = toErl 55
  in let lop_5 = BIF.erlang__op_plus [lop_6, offset_0]
  in let rop_8 = BIF.erlang__byte_size__1 [sizebin_4]
  in let taggedsize_10 = BIF.erlang__op_plus [lop_5, rop_8]
  in let rop_12 = toErl 256
  in let
    matchExpr_13 = BIF.erlang__op_lesser [taggedsize_10, rop_12]
  in
    case matchExpr_13 of
      (ErlangAtom "true") ->
        ErlangBinary
          (BIN.concat
             [BIN.fromInt taggedsize_10 (toErl 8) 1 BIN.Big,
              BIN.binPrefix sizebin_4 (BIN.packedSize sizebin_4) 8,
              BIN.binPrefix x_1 (BIN.packedSize x_1) 8])
      _ -> EXC.badmatch matchExpr_13
erlps__add_size__2 [arg_17, arg_18] = EXC.function_clause unit
erlps__add_size__2 args =
  EXC.badarity (ErlangFun 2 erlps__add_size__2) args

erlps__decode__1 :: ErlangFun
erlps__decode__1 [bin_0]
  | (ErlangAtom "true") ==
      (falsifyErrors
         (\ _ ->
            let lop_12 = BIF.erlang__is_binary__1 [bin_0]
            in
              case lop_12 of
                (ErlangAtom "false") -> ErlangAtom "false"
                (ErlangAtom "true") ->
                  let    lop_14 = BIF.erlang__byte_size__1 [bin_0]
                  in let rop_16 = toErl 0
                  in BIF.erlang__op_greater [lop_14, rop_16]
                _ -> EXC.badarg1 lop_12)) =
  let case_1 = erlps__decode_one__1 [bin_0]
  in
    case case_1 of
      (ErlangTuple [x_3, (ErlangBinary binEnd_4)]) | BIN.empty
                                                       binEnd_4 ->
        x_3
      (ErlangTuple [x_5, left_6]) ->
        let
          arg_7 = ErlangTuple [ErlangAtom "trailing", x_5, bin_0, left_6]
        in BIF.erlang__error__1 [arg_7]
      something_else -> EXC.case_clause something_else
erlps__decode__1 [arg_17] = EXC.function_clause unit
erlps__decode__1 args =
  EXC.badarity (ErlangFun 1 erlps__decode__1) args

erlps__decode_one__1 :: ErlangFun
erlps__decode_one__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok x_3 bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt size_4) <- (BIN.size bin_2)
  , (BIN.Ok b_6 bin_5) <- (BIN.chopBin bin_2 size_4 8)
  , BIN.empty bin_5
  , weakLeq x_3 (toErl 127) =
  let tup_el_7 = ErlangBinary (BIN.fromInt x_3 (toErl 8) 1 BIN.Big)
  in ErlangTuple [tup_el_7, b_6]
erlps__decode_one__1 [b_4@(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok l_3 bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , weakLt l_3 (toErl 192) =
  let    arg_6 = toErl 128
  in let matchExpr_9 = erlps__decode_size__2 [b_4, arg_6]
  in
    case matchExpr_9 of
      (ErlangTuple [size_7, rest_8]) ->
        case rest_8 of
          (ErlangBinary binSeg_10) | (ErlangInt size_11) <- (size_7)
                                   , (BIN.Ok x_13 bin_12) <-
                                       (BIN.chopBin binSeg_10 size_11 8)
                                   , (ErlangInt size_14) <- (BIN.size bin_12)
                                   , (BIN.Ok tail_16 bin_15) <-
                                       (BIN.chopBin bin_12 size_14 8)
                                   , BIN.empty bin_15 ->
            ErlangTuple [x_13, tail_16]
          _ -> EXC.badmatch rest_8
      _ -> EXC.badmatch matchExpr_9
erlps__decode_one__1 [b_1@(ErlangBinary binSeg_0)] =
  let    arg_3 = toErl 192
  in let matchExpr_6 = erlps__decode_size__2 [b_1, arg_3]
  in
    case matchExpr_6 of
      (ErlangTuple [size_4, rest_5]) ->
        case rest_5 of
          (ErlangBinary binSeg_7) | (ErlangInt size_8) <- (size_4)
                                  , (BIN.Ok x_10 bin_9) <-
                                      (BIN.chopBin binSeg_7 size_8 8)
                                  , (ErlangInt size_11) <- (BIN.size bin_9)
                                  , (BIN.Ok tail_13 bin_12) <-
                                      (BIN.chopBin bin_9 size_11 8)
                                  , BIN.empty bin_12 ->
            let tup_el_15 = erlps__decode_list__1 [x_10]
            in ErlangTuple [tup_el_15, tail_13]
          _ -> EXC.badmatch rest_5
      _ -> EXC.badmatch matchExpr_6
erlps__decode_one__1 [arg_18] = EXC.function_clause unit
erlps__decode_one__1 args =
  EXC.badarity (ErlangFun 1 erlps__decode_one__1) args

erlps__decode_size__2 :: ErlangFun
erlps__decode_size__2 [(ErlangBinary binSeg_0), offset_7]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok l_3 bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt size_4) <- (BIN.size bin_2)
  , (BIN.Ok b_6 bin_5) <- (BIN.chopBin bin_2 size_4 8)
  , BIN.empty bin_5
  , (ErlangAtom "true") ==
      (falsifyErrors
         (\ _ ->
            let    rop_15 = toErl 55
            in let rop_13 = BIF.erlang__op_plus [offset_7, rop_15]
            in BIF.erlang__op_lesserEq [l_3, rop_13])) =
  let tup_el_8 = BIF.erlang__op_minus [l_3, offset_7]
  in ErlangTuple [tup_el_8, b_6]
erlps__decode_size__2 [(ErlangBinary binSeg_0), _offset_6]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok _ bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt size_3) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_5) bin_4) <-
      (BIN.chopInt bin_2 size_3 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_5) == (toErl 0) =
  BIF.erlang__error__1 [ErlangAtom "leading_zeroes_in_size"]
erlps__decode_size__2 [(ErlangBinary binSeg_0), offset_7]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok l_3 bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt size_4) <- (BIN.size bin_2)
  , (BIN.Ok b_6 bin_5) <- (BIN.chopBin bin_2 size_4 8)
  , BIN.empty bin_5 =
  let    lop_8 = BIF.erlang__op_minus [l_3, offset_7]
  in let rop_11 = toErl 55
  in let binsize_12 = BIF.erlang__op_minus [lop_8, rop_11]
  in
    case b_6 of
      (ErlangBinary binSeg_13) | (ErlangInt size_14) <- (binsize_12)
                               , (BIN.Ok size_16 bin_15) <-
                                   (BIN.chopInt binSeg_13 size_14 8 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_17) <- (BIN.size bin_15)
                               , (BIN.Ok rest_19 bin_18) <-
                                   (BIN.chopBin bin_15 size_17 8)
                               , BIN.empty bin_18 ->
        ErlangTuple [size_16, rest_19]
      _ -> EXC.badmatch b_6
erlps__decode_size__2 [arg_23, arg_24] = EXC.function_clause unit
erlps__decode_size__2 args =
  EXC.badarity (ErlangFun 2 erlps__decode_size__2) args

erlps__decode_list__1 :: ErlangFun
erlps__decode_list__1 [(ErlangBinary binEnd_0)]
  | BIN.empty binEnd_0 =
  ErlangEmptyList
erlps__decode_list__1 [b_0] =
  let matchExpr_4 = erlps__decode_one__1 [b_0]
  in
    case matchExpr_4 of
      (ErlangTuple [element_2, rest_3]) ->
        let tail_6 = erlps__decode_list__1 [rest_3]
        in ErlangCons element_2 tail_6
      _ -> EXC.badmatch matchExpr_4
erlps__decode_list__1 [arg_8] = EXC.function_clause unit
erlps__decode_list__1 args =
  EXC.badarity (ErlangFun 1 erlps__decode_list__1) args