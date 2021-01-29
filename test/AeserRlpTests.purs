module Aeser.Rlp.Tests(erlps__test__0,
                       erlps__rlp_one_byte_test__0,
                       erlps__rlp_another_one_byte_test__0,
                       erlps__rlp_zero_bytes_test__0,
                       erlps__rlp_two_bytes_test__0,
                       erlps__rlp_one_byte_size_bytes_test__0,
                       erlps__rlp_tagged_size_one_byte_bytes_test__0,
                       erlps__rlp_tagged_size_two_bytes_bytes_test__0,
                       erlps__rlp_zero_bytes_list_test__0,
                       erlps__rlp_one_byte_list_test__0,
                       erlps__rlp_byte_array_list_test__0,
                       erlps__rlp_byte_array_tagged_size_one_byte_list_test__0,
                       erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0,
                       erlps__illegal_size_encoding_list_test__0,
                       erlps__illegal_size_encoding_byte_array_test__0) where
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


erlps__rlp_one_byte_test__0 :: ErlangFun
erlps__rlp_one_byte_test__0 [] =
  let    bin_el_0 = toErl 42
  in let
    b_1 = ErlangBinary (BIN.fromInt bin_el_0 (toErl 8) 1 BIN.Big)
  in let
    matchExpr_4 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [b_1]
  in
    case matchExpr_4 of
      b_3 | (b_3 == b_1) ->
        let
          match_final_5_7 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1" [b_1]
        in
          case match_final_5_7 of
            b_8 | (b_8 == b_1) -> match_final_5_7
            _ -> EXC.badmatch match_final_5_7
      _ -> EXC.badmatch matchExpr_4
erlps__rlp_one_byte_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_another_one_byte_test__0 :: ErlangFun
erlps__rlp_another_one_byte_test__0 [] =
  let    bin_el_0 = toErl 127
  in let
    b_1 = ErlangBinary (BIN.fromInt bin_el_0 (toErl 8) 1 BIN.Big)
  in let
    matchExpr_4 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [b_1]
  in
    case matchExpr_4 of
      b_3 | (b_3 == b_1) ->
        let
          match_final_5_7 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1" [b_1]
        in
          case match_final_5_7 of
            b_8 | (b_8 == b_1) -> match_final_5_7
            _ -> EXC.badmatch match_final_5_7
      _ -> EXC.badmatch matchExpr_4
erlps__rlp_another_one_byte_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_zero_bytes_test__0 :: ErlangFun
erlps__rlp_zero_bytes_test__0 [] =
  let    b_0 = ErlangBinary (BIN.concat [])
  in let lop_1 = toErl 128
  in let rop_2 = toErl 0
  in let s_3 = BIF.erlang__op_plus [lop_1, rop_2]
  in let
    match_final_4_6 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [b_0]
  in
    case match_final_4_6 of
      (ErlangBinary binSeg_7) | (ErlangInt size_8) <- (toErl 8)
                              , (BIN.Ok s_10 bin_9) <-
                                  (BIN.chopInt binSeg_7 size_8 1 BIN.Big
                                     BIN.Unsigned)
                              , (ErlangInt size_11) <- (BIN.size bin_9)
                              , (BIN.Ok b_13 bin_12) <-
                                  (BIN.chopBin bin_9 size_11 8)
                              , BIN.empty bin_12
                              , (s_10 == s_3)
                              , (b_13 == b_0) ->
        match_final_4_6
      _ -> EXC.badmatch match_final_4_6
erlps__rlp_zero_bytes_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_two_bytes_test__0 :: ErlangFun
erlps__rlp_two_bytes_test__0 [] =
  let    bin_el_0 = toErl 128
  in let
    b_1 = ErlangBinary (BIN.fromInt bin_el_0 (toErl 8) 1 BIN.Big)
  in let lop_2 = toErl 128
  in let rop_3 = toErl 1
  in let s_4 = BIF.erlang__op_plus [lop_2, rop_3]
  in let
    match_final_5_7 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [b_1]
  in
    case match_final_5_7 of
      (ErlangBinary binSeg_8) | (ErlangInt size_9) <- (toErl 8)
                              , (BIN.Ok s_11 bin_10) <-
                                  (BIN.chopInt binSeg_8 size_9 1 BIN.Big
                                     BIN.Unsigned)
                              , (ErlangInt size_12) <- (BIN.size bin_10)
                              , (BIN.Ok b_14 bin_13) <-
                                  (BIN.chopBin bin_10 size_12 8)
                              , BIN.empty bin_13
                              , (s_11 == s_4)
                              , (b_14 == b_1) ->
        match_final_5_7
      _ -> EXC.badmatch match_final_5_7
erlps__rlp_two_bytes_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_one_byte_size_bytes_test__0 :: ErlangFun
erlps__rlp_one_byte_size_bytes_test__0 [] =
  let    l_0 = toErl 55
  in let lop_1 = toErl 128
  in let s_3 = BIF.erlang__op_plus [lop_1, l_0]
  in let arg_5 = toErl 1
  in let
    lcSrc_4 =
      BIF.do_remote_fun_call "Lists" "erlps__seq__2" [arg_5, l_0]
  in let
    x_11 =
      BIN.concatErl
        (flmap
           (\ lc_8 ->
              let lcRet_9 = ErlangBinary (BIN.fromInt lc_8 (toErl 8) 1 BIN.Big)
              in ErlangCons lcRet_9 ErlangEmptyList)
           lcSrc_4)
  in let
    matchExpr_20 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_11]
  in
    case matchExpr_20 of
      (ErlangBinary binSeg_13) | (ErlangInt size_14) <- (toErl 8)
                               , (BIN.Ok s_16 bin_15) <-
                                   (BIN.chopInt binSeg_13 size_14 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_17) <- (BIN.size bin_15)
                               , (BIN.Ok x_19 bin_18) <-
                                   (BIN.chopBin bin_15 size_17 8)
                               , BIN.empty bin_18
                               , (s_16 == s_3)
                               , (x_19 == x_11) ->
        let
          match_final_22_24 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_20]
        in
          case match_final_22_24 of
            x_25 | (x_25 == x_11) -> match_final_22_24
            _ -> EXC.badmatch match_final_22_24
      _ -> EXC.badmatch matchExpr_20
erlps__rlp_one_byte_size_bytes_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_tagged_size_one_byte_bytes_test__0 :: ErlangFun
erlps__rlp_tagged_size_one_byte_bytes_test__0 [] =
  let    l_0 = toErl 56
  in let lop_2 = toErl 128
  in let rop_3 = toErl 55
  in let lop_1 = BIF.erlang__op_plus [lop_2, rop_3]
  in let rop_4 = toErl 1
  in let tag_5 = BIF.erlang__op_plus [lop_1, rop_4]
  in let arg_8 = toErl 42
  in let
    arg_6 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_8]
  in let x_9 = BIF.erlang__list_to_binary__1 [arg_6]
  in let s_11 = BIF.erlang__byte_size__1 [x_9]
  in let
    matchExpr_23 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_9]
  in
    case matchExpr_23 of
      (ErlangBinary binSeg_13) | (ErlangInt size_14) <- (toErl 8)
                               , (BIN.Ok tag_16 bin_15) <-
                                   (BIN.chopInt binSeg_13 size_14 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_17) <- (toErl 8)
                               , (BIN.Ok s_19 bin_18) <-
                                   (BIN.chopInt bin_15 size_17 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_20) <- (BIN.size bin_18)
                               , (BIN.Ok x_22 bin_21) <-
                                   (BIN.chopBin bin_18 size_20 8)
                               , BIN.empty bin_21
                               , (tag_16 == tag_5)
                               , (s_19 == s_11)
                               , (x_22 == x_9) ->
        let
          match_final_25_27 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_23]
        in
          case match_final_25_27 of
            x_28 | (x_28 == x_9) -> match_final_25_27
            _ -> EXC.badmatch match_final_25_27
      _ -> EXC.badmatch matchExpr_23
erlps__rlp_tagged_size_one_byte_bytes_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_tagged_size_two_bytes_bytes_test__0 :: ErlangFun
erlps__rlp_tagged_size_two_bytes_bytes_test__0 [] =
  let    l_0 = toErl 256
  in let sizesize_1 = toErl 2
  in let lop_3 = toErl 128
  in let rop_4 = toErl 55
  in let lop_2 = BIF.erlang__op_plus [lop_3, rop_4]
  in let tag_6 = BIF.erlang__op_plus [lop_2, sizesize_1]
  in let arg_9 = toErl 42
  in let
    arg_7 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_9]
  in let x_10 = BIF.erlang__list_to_binary__1 [arg_7]
  in let s_12 = BIF.erlang__byte_size__1 [x_10]
  in let
    matchExpr_24 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_10]
  in
    case matchExpr_24 of
      (ErlangBinary binSeg_14) | (ErlangInt size_15) <- (toErl 8)
                               , (BIN.Ok tag_17 bin_16) <-
                                   (BIN.chopInt binSeg_14 size_15 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_18) <- (sizesize_1)
                               , (BIN.Ok s_20 bin_19) <-
                                   (BIN.chopInt bin_16 size_18 8 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_21) <- (BIN.size bin_19)
                               , (BIN.Ok x_23 bin_22) <-
                                   (BIN.chopBin bin_19 size_21 8)
                               , BIN.empty bin_22
                               , (tag_17 == tag_6)
                               , (s_20 == s_12)
                               , (x_23 == x_10) ->
        let
          match_final_26_28 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_24]
        in
          case match_final_26_28 of
            x_29 | (x_29 == x_10) -> match_final_26_28
            _ -> EXC.badmatch match_final_26_28
      _ -> EXC.badmatch matchExpr_24
erlps__rlp_tagged_size_two_bytes_bytes_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_zero_bytes_list_test__0 :: ErlangFun
erlps__rlp_zero_bytes_list_test__0 [] =
  let    l_0 = toErl 0
  in let lop_1 = toErl 192
  in let tag_3 = BIF.erlang__op_plus [lop_1, l_0]
  in let
    matchExpr_10 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1"
        [ErlangEmptyList]
  in
    case matchExpr_10 of
      (ErlangBinary binSeg_6) | (ErlangInt size_7) <- (toErl 8)
                              , (BIN.Ok tag_9 bin_8) <-
                                  (BIN.chopInt binSeg_6 size_7 1 BIN.Big
                                     BIN.Unsigned)
                              , BIN.empty bin_8
                              , (tag_9 == tag_3) ->
        let
          match_final_12_14 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_10]
        in
          case match_final_12_14 of
            x_15 | (x_15 == ErlangEmptyList) -> match_final_12_14
            _ -> EXC.badmatch match_final_12_14
      _ -> EXC.badmatch matchExpr_10
erlps__rlp_zero_bytes_list_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_one_byte_list_test__0 :: ErlangFun
erlps__rlp_one_byte_list_test__0 [] =
  let    l_0 = toErl 1
  in let lop_1 = toErl 192
  in let tag_3 = BIF.erlang__op_plus [lop_1, l_0]
  in let bin_el_6 = toErl 42
  in let
    arg_5 = ErlangBinary (BIN.fromInt bin_el_6 (toErl 8) 1 BIN.Big)
  in let
    x_7 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_5]
  in let
    matchExpr_16 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_7]
  in
    case matchExpr_16 of
      (ErlangBinary binSeg_9) | (ErlangInt size_10) <- (toErl 8)
                              , (BIN.Ok tag_12 bin_11) <-
                                  (BIN.chopInt binSeg_9 size_10 1 BIN.Big
                                     BIN.Unsigned)
                              , (ErlangInt size_13) <- (toErl 8)
                              , (BIN.Ok (ErlangInt num_15) bin_14) <-
                                  (BIN.chopInt bin_11 size_13 1 BIN.Big
                                     BIN.Unsigned)
                              , ((ErlangInt num_15) == (toErl 42))
                              , BIN.empty bin_14
                              , (tag_12 == tag_3) ->
        let
          match_final_18_20 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_16]
        in
          case match_final_18_20 of
            x_21 | (x_21 == x_7) -> match_final_18_20
            _ -> EXC.badmatch match_final_18_20
      _ -> EXC.badmatch matchExpr_16
erlps__rlp_one_byte_list_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_byte_array_list_test__0 :: ErlangFun
erlps__rlp_byte_array_list_test__0 [] =
  let    l_0 = toErl 55
  in let lop_1 = toErl 192
  in let tag_3 = BIF.erlang__op_plus [lop_1, l_0]
  in let bin_el_6 = toErl 42
  in let
    arg_5 = ErlangBinary (BIN.fromInt bin_el_6 (toErl 8) 1 BIN.Big)
  in let
    x_7 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_5]
  in let y_9 = BIF.erlang__list_to_binary__1 [x_7]
  in let
    matchExpr_18 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_7]
  in
    case matchExpr_18 of
      (ErlangBinary binSeg_11) | (ErlangInt size_12) <- (toErl 8)
                               , (BIN.Ok tag_14 bin_13) <-
                                   (BIN.chopInt binSeg_11 size_12 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_15) <- (BIN.size bin_13)
                               , (BIN.Ok y_17 bin_16) <-
                                   (BIN.chopBin bin_13 size_15 8)
                               , BIN.empty bin_16
                               , (tag_14 == tag_3)
                               , (y_17 == y_9) ->
        let
          match_final_20_22 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_18]
        in
          case match_final_20_22 of
            x_23 | (x_23 == x_7) -> match_final_20_22
            _ -> EXC.badmatch match_final_20_22
      _ -> EXC.badmatch matchExpr_18
erlps__rlp_byte_array_list_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_byte_array_tagged_size_one_byte_list_test__0 :: ErlangFun
erlps__rlp_byte_array_tagged_size_one_byte_list_test__0 [] =
  let    l_0 = toErl 56
  in let sizesize_1 = toErl 1
  in let lop_3 = toErl 192
  in let rop_4 = toErl 55
  in let lop_2 = BIF.erlang__op_plus [lop_3, rop_4]
  in let tag_6 = BIF.erlang__op_plus [lop_2, sizesize_1]
  in let bin_el_9 = toErl 42
  in let
    arg_8 = ErlangBinary (BIN.fromInt bin_el_9 (toErl 8) 1 BIN.Big)
  in let
    x_10 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_8]
  in let y_12 = BIF.erlang__list_to_binary__1 [x_10]
  in let s_14 = BIF.erlang__byte_size__1 [y_12]
  in let
    matchExpr_26 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_10]
  in
    case matchExpr_26 of
      (ErlangBinary binSeg_16) | (ErlangInt size_17) <- (toErl 8)
                               , (BIN.Ok tag_19 bin_18) <-
                                   (BIN.chopInt binSeg_16 size_17 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_20) <- (sizesize_1)
                               , (BIN.Ok s_22 bin_21) <-
                                   (BIN.chopInt bin_18 size_20 8 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_23) <- (BIN.size bin_21)
                               , (BIN.Ok y_25 bin_24) <-
                                   (BIN.chopBin bin_21 size_23 8)
                               , BIN.empty bin_24
                               , (tag_19 == tag_6)
                               , (s_22 == s_14)
                               , (y_25 == y_12) ->
        let
          match_final_28_30 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_26]
        in
          case match_final_28_30 of
            x_31 | (x_31 == x_10) -> match_final_28_30
            _ -> EXC.badmatch match_final_28_30
      _ -> EXC.badmatch matchExpr_26
erlps__rlp_byte_array_tagged_size_one_byte_list_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0 :: ErlangFun
erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0 [] =
  let    l_0 = toErl 256
  in let sizesize_1 = toErl 2
  in let lop_3 = toErl 192
  in let rop_4 = toErl 55
  in let lop_2 = BIF.erlang__op_plus [lop_3, rop_4]
  in let tag_6 = BIF.erlang__op_plus [lop_2, sizesize_1]
  in let bin_el_9 = toErl 42
  in let
    arg_8 = ErlangBinary (BIN.fromInt bin_el_9 (toErl 8) 1 BIN.Big)
  in let
    x_10 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_8]
  in let y_12 = BIF.erlang__list_to_binary__1 [x_10]
  in let s_14 = BIF.erlang__byte_size__1 [y_12]
  in let
    matchExpr_26 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_10]
  in
    case matchExpr_26 of
      (ErlangBinary binSeg_16) | (ErlangInt size_17) <- (toErl 8)
                               , (BIN.Ok tag_19 bin_18) <-
                                   (BIN.chopInt binSeg_16 size_17 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_20) <- (sizesize_1)
                               , (BIN.Ok s_22 bin_21) <-
                                   (BIN.chopInt bin_18 size_20 8 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_23) <- (BIN.size bin_21)
                               , (BIN.Ok y_25 bin_24) <-
                                   (BIN.chopBin bin_21 size_23 8)
                               , BIN.empty bin_24
                               , (tag_19 == tag_6)
                               , (s_22 == s_14)
                               , (y_25 == y_12) ->
        let
          match_final_28_30 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_26]
        in
          case match_final_28_30 of
            x_31 | (x_31 == x_10) -> match_final_28_30
            _ -> EXC.badmatch match_final_28_30
      _ -> EXC.badmatch matchExpr_26
erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__illegal_size_encoding_list_test__0 :: ErlangFun
erlps__illegal_size_encoding_list_test__0 [] =
  let    l_0 = toErl 56
  in let sizesize_1 = toErl 1
  in let lop_3 = toErl 192
  in let rop_4 = toErl 55
  in let lop_2 = BIF.erlang__op_plus [lop_3, rop_4]
  in let tag_6 = BIF.erlang__op_plus [lop_2, sizesize_1]
  in let bin_el_9 = toErl 42
  in let
    arg_8 = ErlangBinary (BIN.fromInt bin_el_9 (toErl 8) 1 BIN.Big)
  in let
    x_10 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_8]
  in let y_12 = BIF.erlang__list_to_binary__1 [x_10]
  in let s_14 = BIF.erlang__byte_size__1 [y_12]
  in let
    matchExpr_26 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_10]
  in
    case matchExpr_26 of
      (ErlangBinary binSeg_16) | (ErlangInt size_17) <- (toErl 8)
                               , (BIN.Ok tag_19 bin_18) <-
                                   (BIN.chopInt binSeg_16 size_17 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_20) <- (sizesize_1)
                               , (BIN.Ok s_22 bin_21) <-
                                   (BIN.chopInt bin_18 size_20 8 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_23) <- (BIN.size bin_21)
                               , (BIN.Ok y_25 bin_24) <-
                                   (BIN.chopBin bin_21 size_23 8)
                               , BIN.empty bin_24
                               , (tag_19 == tag_6)
                               , (s_22 == s_14)
                               , (y_25 == y_12) ->
        let
          matchExpr_30 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_26]
        in
          case matchExpr_30 of
            x_29 | (x_29 == x_10) ->
              let    rop_33 = toErl 1
              in let bin_el_31 = BIF.erlang__op_plus [tag_6, rop_33]
              in let bin_el_34 = toErl 0
              in let
                e1_37 =
                  ErlangBinary
                    (BIN.concat
                       [BIN.fromInt bin_el_31 (toErl 8) 1 BIN.Big,
                        BIN.fromInt bin_el_34 (toErl 8) 1 BIN.Big,
                        BIN.fromInt s_14 sizesize_1 8 BIN.Big,
                        BIN.binPrefix y_12 (BIN.packedSize y_12) 8])
              in let
                fun_38 =
                  ErlangFun 0
                    (let
                       lambda_39 [] =
                         EXC.tryOfCatch
                           (\ _ ->
                              BIF.do_remote_fun_call "Aeser.Rlp"
                                "erlps__decode__1" [e1_37])
                           (\ of_41 ->
                              let   
                                head_48 =
                                  ErlangTuple
                                    [ErlangAtom "module",
                                     ErlangAtom "aeser_rlp_tests"]
                              in let tup_el_54 = toErl 117
                              in let
                                head_52 =
                                  ErlangTuple [ErlangAtom "line", tup_el_54]
                              in let
                                tup_el_58 =
                                  toErl "? TEST_MODULE : decode ( E1 )"
                              in let
                                head_56 =
                                  ErlangTuple
                                    [ErlangAtom "expression", tup_el_58]
                              in let lop_63 = toErl "{ "
                              in let lop_65 = toErl "error"
                              in let lop_67 = toErl " , "
                              in let lop_69 = toErl "leading_zeroes_in_size"
                              in let rop_70 = toErl " , [...] }"
                              in let
                                rop_68 = BIF.erlang__op_append [lop_69, rop_70]
                              in let
                                rop_66 = BIF.erlang__op_append [lop_67, rop_68]
                              in let
                                rop_64 = BIF.erlang__op_append [lop_65, rop_66]
                              in let
                                tup_el_62 =
                                  BIF.erlang__op_append [lop_63, rop_64]
                              in let
                                head_60 =
                                  ErlangTuple [ErlangAtom "pattern", tup_el_62]
                              in let
                                head_72 =
                                  ErlangTuple
                                    [ErlangAtom "unexpected_success", of_41]
                              in let
                                arg_45 =
                                  ErlangTuple
                                    [ErlangAtom "assertException",
                                     ErlangCons head_48
                                       (ErlangCons head_52
                                          (ErlangCons head_56
                                             (ErlangCons head_60
                                                (ErlangCons head_72
                                                   ErlangEmptyList))))]
                              in BIF.erlang__error__1 [arg_45])
                           (\ ex_42 ->
                              case ex_42 of
                                (ErlangTuple [(ErlangAtom "error"),
                                              (ErlangAtom "leading_zeroes_in_size"),
                                              _]) ->
                                  ErlangAtom "ok"
                                (ErlangTuple [__c_76, __t_77, __s_78]) ->
                                  let   
                                    head_82 =
                                      ErlangTuple
                                        [ErlangAtom "module",
                                         ErlangAtom "aeser_rlp_tests"]
                                  in let tup_el_88 = toErl 117
                                  in let
                                    head_86 =
                                      ErlangTuple [ErlangAtom "line", tup_el_88]
                                  in let
                                    tup_el_92 =
                                      toErl "? TEST_MODULE : decode ( E1 )"
                                  in let
                                    head_90 =
                                      ErlangTuple
                                        [ErlangAtom "expression", tup_el_92]
                                  in let lop_97 = toErl "{ "
                                  in let lop_99 = toErl "error"
                                  in let lop_101 = toErl " , "
                                  in let
                                    lop_103 = toErl "leading_zeroes_in_size"
                                  in let rop_104 = toErl " , [...] }"
                                  in let
                                    rop_102 =
                                      BIF.erlang__op_append [lop_103, rop_104]
                                  in let
                                    rop_100 =
                                      BIF.erlang__op_append [lop_101, rop_102]
                                  in let
                                    rop_98 =
                                      BIF.erlang__op_append [lop_99, rop_100]
                                  in let
                                    tup_el_96 =
                                      BIF.erlang__op_append [lop_97, rop_98]
                                  in let
                                    head_94 =
                                      ErlangTuple
                                        [ErlangAtom "pattern", tup_el_96]
                                  in let
                                    tup_el_108 =
                                      ErlangTuple [__c_76, __t_77, __s_78]
                                  in let
                                    head_106 =
                                      ErlangTuple
                                        [ErlangAtom "unexpected_exception",
                                         tup_el_108]
                                  in let
                                    arg_79 =
                                      ErlangTuple
                                        [ErlangAtom "assertException",
                                         ErlangCons head_82
                                           (ErlangCons head_86
                                              (ErlangCons head_90
                                                 (ErlangCons head_94
                                                    (ErlangCons head_106
                                                       ErlangEmptyList))))]
                                  in BIF.erlang__error__1 [arg_79]
                                ex_43 -> EXC.raise ex_43)
                       lambda_39 [] = EXC.function_clause unit
                       lambda_39 args =
                         EXC.badarity (ErlangFun 0 lambda_39) args
                     in lambda_39)
              in BIF.erlang__apply__2 [fun_38, ErlangEmptyList]
            _ -> EXC.badmatch matchExpr_30
      _ -> EXC.badmatch matchExpr_26
erlps__illegal_size_encoding_list_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__illegal_size_encoding_byte_array_test__0 :: ErlangFun
erlps__illegal_size_encoding_byte_array_test__0 [] =
  let    l_0 = toErl 256
  in let sizesize_1 = toErl 2
  in let lop_3 = toErl 128
  in let rop_4 = toErl 55
  in let lop_2 = BIF.erlang__op_plus [lop_3, rop_4]
  in let tag_6 = BIF.erlang__op_plus [lop_2, sizesize_1]
  in let arg_9 = toErl 42
  in let
    arg_7 =
      BIF.do_remote_fun_call "Lists" "erlps__duplicate__2" [l_0, arg_9]
  in let x_10 = BIF.erlang__list_to_binary__1 [arg_7]
  in let s_12 = BIF.erlang__byte_size__1 [x_10]
  in let
    matchExpr_24 =
      BIF.do_remote_fun_call "Aeser.Rlp" "erlps__encode__1" [x_10]
  in
    case matchExpr_24 of
      (ErlangBinary binSeg_14) | (ErlangInt size_15) <- (toErl 8)
                               , (BIN.Ok tag_17 bin_16) <-
                                   (BIN.chopInt binSeg_14 size_15 1 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_18) <- (sizesize_1)
                               , (BIN.Ok s_20 bin_19) <-
                                   (BIN.chopInt bin_16 size_18 8 BIN.Big
                                      BIN.Unsigned)
                               , (ErlangInt size_21) <- (BIN.size bin_19)
                               , (BIN.Ok x_23 bin_22) <-
                                   (BIN.chopBin bin_19 size_21 8)
                               , BIN.empty bin_22
                               , (tag_17 == tag_6)
                               , (s_20 == s_12)
                               , (x_23 == x_10) ->
        let
          matchExpr_28 =
            BIF.do_remote_fun_call "Aeser.Rlp" "erlps__decode__1"
              [matchExpr_24]
        in
          case matchExpr_28 of
            x_27 | (x_27 == x_10) ->
              let    rop_31 = toErl 1
              in let bin_el_29 = BIF.erlang__op_plus [tag_6, rop_31]
              in let bin_el_32 = toErl 0
              in let
                e1_35 =
                  ErlangBinary
                    (BIN.concat
                       [BIN.fromInt bin_el_29 (toErl 8) 1 BIN.Big,
                        BIN.fromInt bin_el_32 (toErl 8) 1 BIN.Big,
                        BIN.fromInt s_12 sizesize_1 8 BIN.Big,
                        BIN.binPrefix x_10 (BIN.packedSize x_10) 8])
              in let
                fun_36 =
                  ErlangFun 0
                    (let
                       lambda_37 [] =
                         EXC.tryOfCatch
                           (\ _ ->
                              BIF.do_remote_fun_call "Aeser.Rlp"
                                "erlps__decode__1" [e1_35])
                           (\ of_39 ->
                              let   
                                head_46 =
                                  ErlangTuple
                                    [ErlangAtom "module",
                                     ErlangAtom "aeser_rlp_tests"]
                              in let tup_el_52 = toErl 131
                              in let
                                head_50 =
                                  ErlangTuple [ErlangAtom "line", tup_el_52]
                              in let
                                tup_el_56 =
                                  toErl "? TEST_MODULE : decode ( E1 )"
                              in let
                                head_54 =
                                  ErlangTuple
                                    [ErlangAtom "expression", tup_el_56]
                              in let lop_61 = toErl "{ "
                              in let lop_63 = toErl "error"
                              in let lop_65 = toErl " , "
                              in let lop_67 = toErl "leading_zeroes_in_size"
                              in let rop_68 = toErl " , [...] }"
                              in let
                                rop_66 = BIF.erlang__op_append [lop_67, rop_68]
                              in let
                                rop_64 = BIF.erlang__op_append [lop_65, rop_66]
                              in let
                                rop_62 = BIF.erlang__op_append [lop_63, rop_64]
                              in let
                                tup_el_60 =
                                  BIF.erlang__op_append [lop_61, rop_62]
                              in let
                                head_58 =
                                  ErlangTuple [ErlangAtom "pattern", tup_el_60]
                              in let
                                head_70 =
                                  ErlangTuple
                                    [ErlangAtom "unexpected_success", of_39]
                              in let
                                arg_43 =
                                  ErlangTuple
                                    [ErlangAtom "assertException",
                                     ErlangCons head_46
                                       (ErlangCons head_50
                                          (ErlangCons head_54
                                             (ErlangCons head_58
                                                (ErlangCons head_70
                                                   ErlangEmptyList))))]
                              in BIF.erlang__error__1 [arg_43])
                           (\ ex_40 ->
                              case ex_40 of
                                (ErlangTuple [(ErlangAtom "error"),
                                              (ErlangAtom "leading_zeroes_in_size"),
                                              _]) ->
                                  ErlangAtom "ok"
                                (ErlangTuple [__c_74, __t_75, __s_76]) ->
                                  let   
                                    head_80 =
                                      ErlangTuple
                                        [ErlangAtom "module",
                                         ErlangAtom "aeser_rlp_tests"]
                                  in let tup_el_86 = toErl 131
                                  in let
                                    head_84 =
                                      ErlangTuple [ErlangAtom "line", tup_el_86]
                                  in let
                                    tup_el_90 =
                                      toErl "? TEST_MODULE : decode ( E1 )"
                                  in let
                                    head_88 =
                                      ErlangTuple
                                        [ErlangAtom "expression", tup_el_90]
                                  in let lop_95 = toErl "{ "
                                  in let lop_97 = toErl "error"
                                  in let lop_99 = toErl " , "
                                  in let
                                    lop_101 = toErl "leading_zeroes_in_size"
                                  in let rop_102 = toErl " , [...] }"
                                  in let
                                    rop_100 =
                                      BIF.erlang__op_append [lop_101, rop_102]
                                  in let
                                    rop_98 =
                                      BIF.erlang__op_append [lop_99, rop_100]
                                  in let
                                    rop_96 =
                                      BIF.erlang__op_append [lop_97, rop_98]
                                  in let
                                    tup_el_94 =
                                      BIF.erlang__op_append [lop_95, rop_96]
                                  in let
                                    head_92 =
                                      ErlangTuple
                                        [ErlangAtom "pattern", tup_el_94]
                                  in let
                                    tup_el_106 =
                                      ErlangTuple [__c_74, __t_75, __s_76]
                                  in let
                                    head_104 =
                                      ErlangTuple
                                        [ErlangAtom "unexpected_exception",
                                         tup_el_106]
                                  in let
                                    arg_77 =
                                      ErlangTuple
                                        [ErlangAtom "assertException",
                                         ErlangCons head_80
                                           (ErlangCons head_84
                                              (ErlangCons head_88
                                                 (ErlangCons head_92
                                                    (ErlangCons head_104
                                                       ErlangEmptyList))))]
                                  in BIF.erlang__error__1 [arg_77]
                                ex_41 -> EXC.raise ex_41)
                       lambda_37 [] = EXC.function_clause unit
                       lambda_37 args =
                         EXC.badarity (ErlangFun 0 lambda_37) args
                     in lambda_37)
              in BIF.erlang__apply__2 [fun_36, ErlangEmptyList]
            _ -> EXC.badmatch matchExpr_28
      _ -> EXC.badmatch matchExpr_24
erlps__illegal_size_encoding_byte_array_test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__test__0 :: ErlangFun
erlps__test__0 [] =
  BIF.do_remote_fun_call "Eunit" "erlps__test__1"
    [ErlangAtom "aeser_rlp_tests"]
erlps__test__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args