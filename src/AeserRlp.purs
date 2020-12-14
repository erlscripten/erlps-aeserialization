module Aeser.Rlp(erlps__decode__1, erlps__decode_one__1,
                 erlps__encode__1) where
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


erlps__encode__1 :: ErlangFun
erlps__encode__1 [x_0] =
  (erlps__encode__2 [x_0, ErlangEmptyList])
erlps__encode__1 [arg_3] = (EXC.function_clause unit)
erlps__encode__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__encode__2 :: ErlangFun
erlps__encode__2 [x_4@(ErlangBinary bin_c_0), _opts_5]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok b_3 bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , (BIN.empty bin_2)
  , (weakLeq b_3 (ErlangInt (DBI.fromInt 127))) =
  x_4
erlps__encode__2 [x_0, _opts_1]
  | ((ErlangAtom "true") ==
       (falsifyErrors (\ _ -> (BIF.erlang__is_binary__1 [x_0])))) =
  (erlps__add_size__2 [(ErlangInt (DBI.fromInt 128)), x_0])
erlps__encode__2 [l_0, opts_1] | (isEList l_0) =
  let
    bytearray_9 =
      (BIN.concat_erl
         (flmap
            (\ lc_4 ->
               case lc_4 of
                 x_3 ->
                   let    bin_el_6 = (erlps__encode__2 [x_3, opts_1])
                   in let
                     lc_ret_5 =
                       (ErlangBinary
                          (BIN.format_bin bin_el_6 (BIN.packed_size bin_el_6)
                             8))
                   in (ErlangCons lc_ret_5 ErlangEmptyList)
                 _ -> ErlangEmptyList)
            l_0))
  in
    (erlps__add_size__2 [(ErlangInt (DBI.fromInt 192)), bytearray_9])
erlps__encode__2 [arg_12, arg_13] = (EXC.function_clause unit)
erlps__encode__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__add_size__2 :: ErlangFun
erlps__add_size__2 [offset_0, x_1]
  | ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let lop_7 = (BIF.erlang__byte_size__1 [x_1])
             in
               (BIF.erlang__op_lesserEq
                  [lop_7, (ErlangInt (DBI.fromInt 55))])))) =
  let    rop_4 = (BIF.erlang__byte_size__1 [x_1])
  in let bin_el_2 = (BIF.erlang__op_plus [offset_0, rop_4])
  in
    (ErlangBinary
       (BIN.concat
          [(BIN.from_int bin_el_2 (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
           (BIN.format_bin x_1 (BIN.packed_size x_1) 8)]))
erlps__add_size__2 [offset_0, x_1]
  | ((ErlangAtom "true") ==
       (falsifyErrors (\ _ -> (BIF.erlang__is_binary__1 [x_1])))) =
  let    arg_2 = (BIF.erlang__byte_size__1 [x_1])
  in let
    sizebin_4 =
      (BIF.do_remote_fun_call "Binary" "erlps__encode_unsigned__1"
         [arg_2])
  in let
    lop_5 =
      (BIF.erlang__op_plus [(ErlangInt (DBI.fromInt 55)), offset_0])
  in let rop_8 = (BIF.erlang__byte_size__1 [sizebin_4])
  in let taggedsize_10 = (BIF.erlang__op_plus [lop_5, rop_8])
  in let
    match_expr_13 =
      (BIF.erlang__op_lesser
         [taggedsize_10, (ErlangInt (DBI.fromInt 256))])
  in
    case match_expr_13 of
      (ErlangAtom "true") ->
        (ErlangBinary
           (BIN.concat
              [(BIN.from_int taggedsize_10 (ErlangInt (DBI.fromInt 8)) 1
                  BIN.Big),
               (BIN.format_bin sizebin_4 (BIN.packed_size sizebin_4) 8),
               (BIN.format_bin x_1 (BIN.packed_size x_1) 8)]))
      _ -> (EXC.badmatch match_expr_13)
erlps__add_size__2 [arg_18, arg_19] = (EXC.function_clause unit)
erlps__add_size__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__decode__1 :: ErlangFun
erlps__decode__1 [bin_0]
  | ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let lop_12 = (BIF.erlang__is_binary__1 [bin_0])
             in
               case lop_12 of
                 (ErlangAtom "false") -> (ErlangAtom "false")
                 (ErlangAtom "true") ->
                   let lop_14 = (BIF.erlang__byte_size__1 [bin_0])
                   in
                     (BIF.erlang__op_greater
                        [lop_14, (ErlangInt (DBI.fromInt 0))])
                 _ -> (EXC.badarg1 lop_12)))) =
  let case_1 = (erlps__decode_one__1 [bin_0])
  in
    case case_1 of
      (ErlangTuple [x_3, (ErlangBinary bin_e_4)]) | (BIN.empty
                                                       bin_e_4) ->
        x_3
      (ErlangTuple [x_5, left_6]) ->
        let
          arg_7 =
            (ErlangTuple [(ErlangAtom "trailing"), x_5, bin_0, left_6])
        in (BIF.erlang__error__1 [arg_7])
      something_else -> (EXC.case_clause something_else)
erlps__decode__1 [arg_17] = (EXC.function_clause unit)
erlps__decode__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__decode_one__1 :: ErlangFun
erlps__decode_one__1 [(ErlangBinary bin_c_0)]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok x_3 bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , (ErlangInt size_4) <- ((BIN.size bin_2))
  , (BIN.Ok b_6 bin_5) <- ((BIN.chop_bin bin_2 size_4 8))
  , (BIN.empty bin_5)
  , (weakLeq x_3 (ErlangInt (DBI.fromInt 127))) =
  let
    tup_el_7 =
      (ErlangBinary
         (BIN.from_int x_3 (ErlangInt (DBI.fromInt 8)) 1 BIN.Big))
  in (ErlangTuple [tup_el_7, b_6])
erlps__decode_one__1 [b_6@(ErlangBinary bin_c_0)]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok l_3 bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , (ErlangInt size_4) <- ((BIN.size bin_2))
  , (BIN.Ok _ bin_5) <- ((BIN.chop_bin bin_2 size_4 8))
  , (BIN.empty bin_5)
  , (weakLt l_3 (ErlangInt (DBI.fromInt 192))) =
  let
    match_expr_11 =
      (erlps__decode_size__2 [b_6, (ErlangInt (DBI.fromInt 128))])
  in
    case match_expr_11 of
      (ErlangTuple [size_9, rest_10]) ->
        case rest_10 of
          (ErlangBinary bin_c_12) | (ErlangInt size_13) <- (size_9)
                                  , (BIN.Ok x_15 bin_14) <-
                                      ((BIN.chop_bin bin_c_12 size_13 8))
                                  , (ErlangInt size_16) <- ((BIN.size bin_14))
                                  , (BIN.Ok tail_18 bin_17) <-
                                      ((BIN.chop_bin bin_14 size_16 8))
                                  , (BIN.empty bin_17) ->
            (ErlangTuple [x_15, tail_18])
          _ -> (EXC.badmatch rest_10)
      _ -> (EXC.badmatch match_expr_11)
erlps__decode_one__1 [b_3@(ErlangBinary bin_c_0)]
  | (ErlangInt size_1) <- ((BIN.size bin_c_0))
  , (BIN.Ok _ bin_2) <- ((BIN.chop_bin bin_c_0 size_1 8))
  , (BIN.empty bin_2) =
  let
    match_expr_8 =
      (erlps__decode_size__2 [b_3, (ErlangInt (DBI.fromInt 192))])
  in
    case match_expr_8 of
      (ErlangTuple [size_6, rest_7]) ->
        case rest_7 of
          (ErlangBinary bin_c_9) | (ErlangInt size_10) <- (size_6)
                                 , (BIN.Ok x_12 bin_11) <-
                                     ((BIN.chop_bin bin_c_9 size_10 8))
                                 , (ErlangInt size_13) <- ((BIN.size bin_11))
                                 , (BIN.Ok tail_15 bin_14) <-
                                     ((BIN.chop_bin bin_11 size_13 8))
                                 , (BIN.empty bin_14) ->
            let tup_el_17 = (erlps__decode_list__1 [x_12])
            in (ErlangTuple [tup_el_17, tail_15])
          _ -> (EXC.badmatch rest_7)
      _ -> (EXC.badmatch match_expr_8)
erlps__decode_one__1 [arg_20] = (EXC.function_clause unit)
erlps__decode_one__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__decode_size__2 :: ErlangFun
erlps__decode_size__2 [(ErlangBinary bin_c_0), offset_7]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok l_3 bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , (ErlangInt size_4) <- ((BIN.size bin_2))
  , (BIN.Ok b_6 bin_5) <- ((BIN.chop_bin bin_2 size_4 8))
  , (BIN.empty bin_5)
  , ((ErlangAtom "true") ==
       (falsifyErrors
          (\ _ ->
             let
               rop_13 =
                 (BIF.erlang__op_plus [offset_7, (ErlangInt (DBI.fromInt 55))])
             in (BIF.erlang__op_lesserEq [l_3, rop_13])))) =
  let tup_el_8 = (BIF.erlang__op_minus [l_3, offset_7])
  in (ErlangTuple [tup_el_8, b_6])
erlps__decode_size__2 [(ErlangBinary bin_c_0), _offset_8]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok _ bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , size_3 <- ((DBI.fromInt 8))
  , (BIN.Ok (ErlangInt num_5) bin_4) <-
      ((BIN.chop_int bin_2 size_3 1 BIN.Big BIN.Unsigned))
  , ((ErlangInt num_5) == (ErlangInt (DBI.fromInt 0)))
  , (ErlangInt size_6) <- ((BIN.size bin_4))
  , (BIN.Ok _ bin_7) <- ((BIN.chop_bin bin_4 size_6 8))
  , (BIN.empty bin_7) =
  (BIF.erlang__error__1 [(ErlangAtom "leading_zeroes_in_size")])
erlps__decode_size__2 [(ErlangBinary bin_c_0), offset_7]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok l_3 bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , (ErlangInt size_4) <- ((BIN.size bin_2))
  , (BIN.Ok b_6 bin_5) <- ((BIN.chop_bin bin_2 size_4 8))
  , (BIN.empty bin_5) =
  let    lop_8 = (BIF.erlang__op_minus [l_3, offset_7])
  in let
    binsize_12 =
      (BIF.erlang__op_minus [lop_8, (ErlangInt (DBI.fromInt 55))])
  in
    case b_6 of
      (ErlangBinary bin_c_13) | (ErlangInt size_14) <- (binsize_12)
                              , (BIN.Ok size_16 bin_15) <-
                                  ((BIN.chop_int bin_c_13 size_14 8 BIN.Big
                                      BIN.Unsigned))
                              , (ErlangInt size_17) <- ((BIN.size bin_15))
                              , (BIN.Ok rest_19 bin_18) <-
                                  ((BIN.chop_bin bin_15 size_17 8))
                              , (BIN.empty bin_18) ->
        (ErlangTuple [size_16, rest_19])
      _ -> (EXC.badmatch b_6)
erlps__decode_size__2 [arg_23, arg_24] =
  (EXC.function_clause unit)
erlps__decode_size__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__decode_list__1 :: ErlangFun
erlps__decode_list__1 [(ErlangBinary bin_e_0)]
  | (BIN.empty bin_e_0) =
  ErlangEmptyList
erlps__decode_list__1 [b_0] =
  let match_expr_4 = (erlps__decode_one__1 [b_0])
  in
    case match_expr_4 of
      (ErlangTuple [element_2, rest_3]) ->
        let tail_6 = (erlps__decode_list__1 [rest_3])
        in (ErlangCons element_2 tail_6)
      _ -> (EXC.badmatch match_expr_4)
erlps__decode_list__1 [arg_8] = (EXC.function_clause unit)
erlps__decode_list__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)