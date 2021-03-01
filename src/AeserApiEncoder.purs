module Aeser.Api.Encoder(erlps__encode__2, erlps__decode__1,
                         erlps__safe_decode__2,
                         erlps__byte_size_for_type__1) where
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


erlps__encode__2 :: ErlangFun
erlps__encode__2 [(ErlangAtom "id_hash"), payload_0] =
  let
    matchExpr_4 =
      BIF.do_remote_fun_call "Aeser.Id" "erlps__specialize__1"
        [payload_0]
  in
    case matchExpr_4 of
      (ErlangTuple [idtype_2, val_3]) ->
        let arg_5 = erlps__id2type__1 [idtype_2]
        in erlps__encode__2 [arg_5, val_3]
      _ -> EXC.badmatch matchExpr_4
erlps__encode__2 [type_0, payload_1] =
  let    pfx_3 = erlps__type2pfx__1 [type_0]
  in let case_4 = erlps__type2enc__1 [type_0]
  in let
    enc_10 =
      case case_4 of
        (ErlangInt num_6) | (ErlangInt num_6) == (toErl 1) ->
          erlps__base58_check__1 [payload_1]
        (ErlangInt num_8) | (ErlangInt num_8) == (toErl 2) ->
          erlps__base64_check__1 [payload_1]
        something_else -> EXC.case_clause something_else
  in
    ErlangBinary
      (BIN.concat
         [BIN.binPrefix pfx_3 (BIN.packedSize pfx_3) 8,
          BIN.fromInts (toErl "_") (toErl 8) 1 BIN.Big,
          BIN.binPrefix enc_10 (BIN.packedSize enc_10) 8])
erlps__encode__2 [arg_13, arg_14] = EXC.function_clause unit
erlps__encode__2 args =
  EXC.badarity (ErlangFun 2 erlps__encode__2) args

erlps__decode__1 :: ErlangFun
erlps__decode__1 [bin0_0] =
  let case_1 = erlps__split__1 [bin0_0]
  in
    case case_1 of
      (ErlangCons pfx_3 (ErlangCons payload_4 (ErlangEmptyList))) ->
        let    type_6 = erlps__pfx2type__1 [pfx_3]
        in let bin_9 = erlps__decode_check__2 [type_6, payload_4]
        in let case_10 = erlps__type_size_check__2 [type_6, bin_9]
        in
          case case_10 of
            (ErlangAtom "ok") -> ErlangTuple [type_6, bin_9]
            (ErlangTuple [(ErlangAtom "error"), reason_15]) ->
              BIF.erlang__error__1 [reason_15]
            something_else -> EXC.case_clause something_else
      _ -> BIF.erlang__error__1 [ErlangAtom "missing_prefix"]
erlps__decode__1 [arg_18] = EXC.function_clause unit
erlps__decode__1 args =
  EXC.badarity (ErlangFun 1 erlps__decode__1) args

erlps__type_size_check__2 :: ErlangFun
erlps__type_size_check__2 [type_0, bin_1] =
  let case_2 = erlps__byte_size_for_type__1 [type_0]
  in
    case case_2 of
      (ErlangAtom "not_applicable") -> ErlangAtom "ok"
      correctsize_4 ->
        let    size_6 = BIF.erlang__byte_size__1 [bin_1]
        in let case_7 = BIF.erlang__op_exactEq [size_6, correctsize_4]
        in
          case case_7 of
            (ErlangAtom "true") -> ErlangAtom "ok"
            (ErlangAtom "false") ->
              ErlangTuple [ErlangAtom "error", ErlangAtom "incorrect_size"]
            something_else -> EXC.case_clause something_else
erlps__type_size_check__2 [arg_12, arg_13] =
  EXC.function_clause unit
erlps__type_size_check__2 args =
  EXC.badarity (ErlangFun 2 erlps__type_size_check__2) args

erlps__safe_decode__2 :: ErlangFun
erlps__safe_decode__2 [(ErlangTuple [(ErlangAtom "id_hash"),
                                     allowedtypes_0]),
                       enc_1]
  =
  EXC.tryOfCatch (\ _ -> erlps__decode__1 [enc_1])
    (\ of_3 ->
       case of_3 of
         (ErlangTuple [actualtype_6, dec_7]) ->
           let case_8 = BIF.lists__member__2 [actualtype_6, allowedtypes_0]
           in
             case case_8 of
               (ErlangAtom "true") ->
                 EXC.tryCatch
                   (\ _ ->
                      let    arg_13 = erlps__type2id__1 [actualtype_6]
                      in let
                        tup_el_12 =
                          BIF.do_remote_fun_call "Aeser.Id" "erlps__create__2"
                            [arg_13, dec_7]
                      in ErlangTuple [ErlangAtom "ok", tup_el_12])
                   (\ ex_17 ->
                      case ex_17 of
                        (ErlangTuple [(ErlangAtom "error"), _, _]) ->
                          ErlangTuple
                            [ErlangAtom "error", ErlangAtom "invalid_prefix"]
                        ex_18 -> EXC.raise ex_18)
               (ErlangAtom "false") ->
                 ErlangTuple [ErlangAtom "error", ErlangAtom "invalid_prefix"]
               something_else -> EXC.case_clause something_else
         something_else -> EXC.try_clause something_else)
    (\ ex_4 ->
       case ex_4 of
         (ErlangTuple [(ErlangAtom "error"), _, _]) ->
           ErlangTuple [ErlangAtom "error", ErlangAtom "invalid_encoding"]
         ex_5 -> EXC.raise ex_5)
erlps__safe_decode__2 [(ErlangAtom "block_hash"), enc_0] =
  EXC.tryOfCatch (\ _ -> erlps__decode__1 [enc_0])
    (\ of_2 ->
       case of_2 of
         (ErlangTuple [(ErlangAtom "key_block_hash"), dec_5]) ->
           ErlangTuple [ErlangAtom "ok", dec_5]
         (ErlangTuple [(ErlangAtom "micro_block_hash"), dec_8]) ->
           ErlangTuple [ErlangAtom "ok", dec_8]
         (ErlangTuple [_, _]) ->
           ErlangTuple [ErlangAtom "error", ErlangAtom "invalid_prefix"]
         something_else -> EXC.try_clause something_else)
    (\ ex_3 ->
       case ex_3 of
         (ErlangTuple [(ErlangAtom "error"), _, _]) ->
           ErlangTuple [ErlangAtom "error", ErlangAtom "invalid_encoding"]
         ex_4 -> EXC.raise ex_4)
erlps__safe_decode__2 [type_0, enc_1] =
  EXC.tryOfCatch (\ _ -> erlps__decode__1 [enc_1])
    (\ of_3 ->
       case of_3 of
         (ErlangTuple [type_6, dec_7]) | type_6 == type_0 ->
           ErlangTuple [ErlangAtom "ok", dec_7]
         (ErlangTuple [_, _]) ->
           ErlangTuple [ErlangAtom "error", ErlangAtom "invalid_prefix"]
         something_else -> EXC.try_clause something_else)
    (\ ex_4 ->
       case ex_4 of
         (ErlangTuple [(ErlangAtom "error"), _, _]) ->
           ErlangTuple [ErlangAtom "error", ErlangAtom "invalid_encoding"]
         ex_5 -> EXC.raise ex_5)
erlps__safe_decode__2 [arg_14, arg_15] = EXC.function_clause unit
erlps__safe_decode__2 args =
  EXC.badarity (ErlangFun 2 erlps__safe_decode__2) args

erlps__decode_check__2 :: ErlangFun
erlps__decode_check__2 [type_0, bin_1] =
  let    case_2 = erlps__type2enc__1 [type_0]
  in let
    dec_8 =
      case case_2 of
        (ErlangInt num_4) | (ErlangInt num_4) == (toErl 1) ->
          erlps__base58_to_binary__1 [bin_1]
        (ErlangInt num_6) | (ErlangInt num_6) == (toErl 2) ->
          erlps__base64_to_binary__1 [bin_1]
        something_else -> EXC.case_clause something_else
  in let sz_10 = BIF.erlang__byte_size__1 [dec_8]
  in let rop_12 = toErl 4
  in let bsz_13 = BIF.erlang__op_minus [sz_10, rop_12]
  in
    case dec_8 of
      (ErlangBinary binSeg_14) | (ErlangInt size_15) <- (bsz_13)
                               , (BIN.Ok body_17 bin_16) <-
                                   (BIN.chopBin binSeg_14 size_15 8)
                               , (ErlangInt size_18) <- (toErl 4)
                               , (BIN.Ok c_20 bin_19) <-
                                   (BIN.chopBin bin_16 size_18 8)
                               , BIN.empty bin_19 ->
        let matchExpr_24 = erlps__check_str__1 [body_17]
        in
          case matchExpr_24 of
            c_23 | c_23 == c_20 -> body_17
            _ -> EXC.badmatch matchExpr_24
      _ -> EXC.badmatch dec_8
erlps__decode_check__2 [arg_25, arg_26] =
  EXC.function_clause unit
erlps__decode_check__2 args =
  EXC.badarity (ErlangFun 2 erlps__decode_check__2) args

erlps__base64_check__1 :: ErlangFun
erlps__base64_check__1 [bin_0] =
  let    c_2 = erlps__check_str__1 [bin_0]
  in let
    arg_3 =
      BIF.erlang__iolist_to_binary__1
        [ErlangCons bin_0 (ErlangCons c_2 ErlangEmptyList)]
  in erlps__binary_to_base64__1 [arg_3]
erlps__base64_check__1 [arg_9] = EXC.function_clause unit
erlps__base64_check__1 args =
  EXC.badarity (ErlangFun 1 erlps__base64_check__1) args

erlps__base58_check__1 :: ErlangFun
erlps__base58_check__1 [bin_0] =
  let    c_2 = erlps__check_str__1 [bin_0]
  in let
    arg_3 =
      BIF.erlang__iolist_to_binary__1
        [ErlangCons bin_0 (ErlangCons c_2 ErlangEmptyList)]
  in erlps__binary_to_base58__1 [arg_3]
erlps__base58_check__1 [arg_9] = EXC.function_clause unit
erlps__base58_check__1 args =
  EXC.badarity (ErlangFun 1 erlps__base58_check__1) args

erlps__split__1 :: ErlangFun
erlps__split__1 [bin_0] =
  let
    head_3 =
      ErlangBinary (BIN.fromInts (toErl "_") (toErl 8) 1 BIN.Big)
  in
    BIF.binary__split__3
      [bin_0, ErlangCons head_3 ErlangEmptyList, ErlangEmptyList]
erlps__split__1 [arg_6] = EXC.function_clause unit
erlps__split__1 args =
  EXC.badarity (ErlangFun 1 erlps__split__1) args

erlps__check_str__1 :: ErlangFun
erlps__check_str__1 [bin_0] =
  let    arg_1 = erlps__sha256_hash__1 [bin_0]
  in let matchExpr_7 = erlps__sha256_hash__1 [arg_1]
  in
    case matchExpr_7 of
      (ErlangBinary binSeg_3) | (ErlangInt size_4) <- (toErl 32)
                              , (BIN.Ok c_6 bin_5) <-
                                  (BIN.chopBin binSeg_3 size_4 1) ->
        c_6
      _ -> EXC.badmatch matchExpr_7
erlps__check_str__1 [arg_8] = EXC.function_clause unit
erlps__check_str__1 args =
  EXC.badarity (ErlangFun 1 erlps__check_str__1) args

erlps__sha256_hash__1 :: ErlangFun
erlps__sha256_hash__1 [bin_0] =
  BIF.do_remote_fun_call "Crypto" "erlps__hash__2"
    [ErlangAtom "sha256", bin_0]
erlps__sha256_hash__1 [arg_3] = EXC.function_clause unit
erlps__sha256_hash__1 args =
  EXC.badarity (ErlangFun 1 erlps__sha256_hash__1) args

erlps__id2type__1 :: ErlangFun
erlps__id2type__1 [(ErlangAtom "account")] =
  ErlangAtom "account_pubkey"
erlps__id2type__1 [(ErlangAtom "channel")] = ErlangAtom "channel"
erlps__id2type__1 [(ErlangAtom "commitment")] =
  ErlangAtom "commitment"
erlps__id2type__1 [(ErlangAtom "contract")] =
  ErlangAtom "contract_pubkey"
erlps__id2type__1 [(ErlangAtom "name")] = ErlangAtom "name"
erlps__id2type__1 [(ErlangAtom "oracle")] =
  ErlangAtom "oracle_pubkey"
erlps__id2type__1 [arg_0] = EXC.function_clause unit
erlps__id2type__1 args =
  EXC.badarity (ErlangFun 1 erlps__id2type__1) args

erlps__type2id__1 :: ErlangFun
erlps__type2id__1 [(ErlangAtom "account_pubkey")] =
  ErlangAtom "account"
erlps__type2id__1 [(ErlangAtom "channel")] = ErlangAtom "channel"
erlps__type2id__1 [(ErlangAtom "commitment")] =
  ErlangAtom "commitment"
erlps__type2id__1 [(ErlangAtom "contract_pubkey")] =
  ErlangAtom "contract"
erlps__type2id__1 [(ErlangAtom "name")] = ErlangAtom "name"
erlps__type2id__1 [(ErlangAtom "oracle_pubkey")] =
  ErlangAtom "oracle"
erlps__type2id__1 [arg_0] = EXC.function_clause unit
erlps__type2id__1 args =
  EXC.badarity (ErlangFun 1 erlps__type2id__1) args

erlps__type2enc__1 :: ErlangFun
erlps__type2enc__1 [(ErlangAtom "key_block_hash")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "micro_block_hash")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "block_pof_hash")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "block_tx_hash")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "block_state_hash")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "channel")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "contract_pubkey")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "contract_bytearray")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "contract_store_key")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "contract_store_value")] =
  toErl 2
erlps__type2enc__1 [(ErlangAtom "transaction")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "tx_hash")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "oracle_pubkey")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "oracle_query")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "oracle_query_id")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "oracle_response")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "account_pubkey")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "signature")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "commitment")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "peer_pubkey")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "name")] = toErl 1
erlps__type2enc__1 [(ErlangAtom "state")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "poi")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "state_trees")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "call_state_tree")] = toErl 2
erlps__type2enc__1 [(ErlangAtom "bytearray")] = toErl 2
erlps__type2enc__1 [arg_0] = EXC.function_clause unit
erlps__type2enc__1 args =
  EXC.badarity (ErlangFun 1 erlps__type2enc__1) args

erlps__type2pfx__1 :: ErlangFun
erlps__type2pfx__1 [(ErlangAtom "key_block_hash")] =
  ErlangBinary (BIN.fromInts (toErl "kh") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "micro_block_hash")] =
  ErlangBinary (BIN.fromInts (toErl "mh") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "block_pof_hash")] =
  ErlangBinary (BIN.fromInts (toErl "bf") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "block_tx_hash")] =
  ErlangBinary (BIN.fromInts (toErl "bx") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "block_state_hash")] =
  ErlangBinary (BIN.fromInts (toErl "bs") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "channel")] =
  ErlangBinary (BIN.fromInts (toErl "ch") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "contract_pubkey")] =
  ErlangBinary (BIN.fromInts (toErl "ct") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "contract_bytearray")] =
  ErlangBinary (BIN.fromInts (toErl "cb") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "contract_store_key")] =
  ErlangBinary (BIN.fromInts (toErl "ck") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "contract_store_value")] =
  ErlangBinary (BIN.fromInts (toErl "cv") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "transaction")] =
  ErlangBinary (BIN.fromInts (toErl "tx") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "tx_hash")] =
  ErlangBinary (BIN.fromInts (toErl "th") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "oracle_pubkey")] =
  ErlangBinary (BIN.fromInts (toErl "ok") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "oracle_query")] =
  ErlangBinary (BIN.fromInts (toErl "ov") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "oracle_query_id")] =
  ErlangBinary (BIN.fromInts (toErl "oq") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "oracle_response")] =
  ErlangBinary (BIN.fromInts (toErl "or") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "account_pubkey")] =
  ErlangBinary (BIN.fromInts (toErl "ak") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "signature")] =
  ErlangBinary (BIN.fromInts (toErl "sg") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "commitment")] =
  ErlangBinary (BIN.fromInts (toErl "cm") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "peer_pubkey")] =
  ErlangBinary (BIN.fromInts (toErl "pp") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "name")] =
  ErlangBinary (BIN.fromInts (toErl "nm") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "state")] =
  ErlangBinary (BIN.fromInts (toErl "st") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "poi")] =
  ErlangBinary (BIN.fromInts (toErl "pi") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "state_trees")] =
  ErlangBinary (BIN.fromInts (toErl "ss") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "call_state_tree")] =
  ErlangBinary (BIN.fromInts (toErl "cs") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [(ErlangAtom "bytearray")] =
  ErlangBinary (BIN.fromInts (toErl "ba") (toErl 8) 1 BIN.Big)
erlps__type2pfx__1 [arg_0] = EXC.function_clause unit
erlps__type2pfx__1 args =
  EXC.badarity (ErlangFun 1 erlps__type2pfx__1) args

erlps__pfx2type__1 :: ErlangFun
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 107)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 104)
  , BIN.empty bin_5 =
  ErlangAtom "key_block_hash"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 109)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 104)
  , BIN.empty bin_5 =
  ErlangAtom "micro_block_hash"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 98)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 102)
  , BIN.empty bin_5 =
  ErlangAtom "block_pof_hash"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 98)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 120)
  , BIN.empty bin_5 =
  ErlangAtom "block_tx_hash"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 98)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 115)
  , BIN.empty bin_5 =
  ErlangAtom "block_state_hash"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 104)
  , BIN.empty bin_5 =
  ErlangAtom "channel"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 98)
  , BIN.empty bin_5 =
  ErlangAtom "contract_bytearray"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 107)
  , BIN.empty bin_5 =
  ErlangAtom "contract_store_key"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 118)
  , BIN.empty bin_5 =
  ErlangAtom "contract_store_value"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 116)
  , BIN.empty bin_5 =
  ErlangAtom "contract_pubkey"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 116)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 120)
  , BIN.empty bin_5 =
  ErlangAtom "transaction"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 116)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 104)
  , BIN.empty bin_5 =
  ErlangAtom "tx_hash"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 111)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 107)
  , BIN.empty bin_5 =
  ErlangAtom "oracle_pubkey"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 111)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 118)
  , BIN.empty bin_5 =
  ErlangAtom "oracle_query"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 111)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 113)
  , BIN.empty bin_5 =
  ErlangAtom "oracle_query_id"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 111)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 114)
  , BIN.empty bin_5 =
  ErlangAtom "oracle_response"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 97)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 107)
  , BIN.empty bin_5 =
  ErlangAtom "account_pubkey"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 115)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 103)
  , BIN.empty bin_5 =
  ErlangAtom "signature"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 109)
  , BIN.empty bin_5 =
  ErlangAtom "commitment"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 112)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 112)
  , BIN.empty bin_5 =
  ErlangAtom "peer_pubkey"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 110)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 109)
  , BIN.empty bin_5 =
  ErlangAtom "name"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 115)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 116)
  , BIN.empty bin_5 =
  ErlangAtom "state"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 112)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 105)
  , BIN.empty bin_5 =
  ErlangAtom "poi"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 115)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 115)
  , BIN.empty bin_5 =
  ErlangAtom "state_trees"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 99)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 115)
  , BIN.empty bin_5 =
  ErlangAtom "call_state_tree"
erlps__pfx2type__1 [(ErlangBinary binSeg_0)]
  | (ErlangInt size_1) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      (BIN.chopInt binSeg_0 size_1 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_3) == (toErl 98)
  , (ErlangInt size_4) <- (toErl 8)
  , (BIN.Ok (ErlangInt num_6) bin_5) <-
      (BIN.chopInt bin_2 size_4 1 BIN.Big BIN.Unsigned)
  , (ErlangInt num_6) == (toErl 97)
  , BIN.empty bin_5 =
  ErlangAtom "bytearray"
erlps__pfx2type__1 [arg_7] = EXC.function_clause unit
erlps__pfx2type__1 args =
  EXC.badarity (ErlangFun 1 erlps__pfx2type__1) args

erlps__byte_size_for_type__1 :: ErlangFun
erlps__byte_size_for_type__1 [(ErlangAtom "key_block_hash")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "micro_block_hash")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "block_pof_hash")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "block_tx_hash")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "block_state_hash")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "channel")] = toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "contract_pubkey")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "contract_bytearray")]
  =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "contract_store_key")]
  =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "contract_store_value")]
  =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "transaction")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "tx_hash")] = toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "oracle_pubkey")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "oracle_query")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "oracle_query_id")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "oracle_response")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "account_pubkey")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "signature")] =
  toErl 64
erlps__byte_size_for_type__1 [(ErlangAtom "name")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "commitment")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "peer_pubkey")] =
  toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "state")] = toErl 32
erlps__byte_size_for_type__1 [(ErlangAtom "poi")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "state_trees")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "call_state_tree")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [(ErlangAtom "bytearray")] =
  ErlangAtom "not_applicable"
erlps__byte_size_for_type__1 [arg_0] = EXC.function_clause unit
erlps__byte_size_for_type__1 args =
  EXC.badarity (ErlangFun 1 erlps__byte_size_for_type__1) args

erlps__binary_to_base58__1 :: ErlangFun
erlps__binary_to_base58__1 [bin_0] =
  let
    arg_1 =
      BIF.do_remote_fun_call "Base58" "erlps__binary_to_base58__1"
        [bin_0]
  in BIF.erlang__iolist_to_binary__1 [arg_1]
erlps__binary_to_base58__1 [arg_3] = EXC.function_clause unit
erlps__binary_to_base58__1 args =
  EXC.badarity (ErlangFun 1 erlps__binary_to_base58__1) args

erlps__base58_to_binary__1 :: ErlangFun
erlps__base58_to_binary__1 [bin_0] | isEBinary bin_0 =
  let arg_1 = BIF.erlang__binary_to_list__1 [bin_0]
  in
    BIF.do_remote_fun_call "Base58" "erlps__base58_to_binary__1"
      [arg_1]
erlps__base58_to_binary__1 [arg_3] = EXC.function_clause unit
erlps__base58_to_binary__1 args =
  EXC.badarity (ErlangFun 1 erlps__base58_to_binary__1) args

erlps__binary_to_base64__1 :: ErlangFun
erlps__binary_to_base64__1 [bin_0] =
  BIF.do_remote_fun_call "Base64" "erlps__encode__1" [bin_0]
erlps__binary_to_base64__1 [arg_2] = EXC.function_clause unit
erlps__binary_to_base64__1 args =
  EXC.badarity (ErlangFun 1 erlps__binary_to_base64__1) args

erlps__base64_to_binary__1 :: ErlangFun
erlps__base64_to_binary__1 [bin_0] | isEBinary bin_0 =
  BIF.do_remote_fun_call "Base64" "erlps__decode__1" [bin_0]
erlps__base64_to_binary__1 [arg_2] = EXC.function_clause unit
erlps__base64_to_binary__1 args =
  EXC.badarity (ErlangFun 1 erlps__base64_to_binary__1) args