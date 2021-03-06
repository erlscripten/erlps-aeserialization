module Aeser.Chain.Objects(erlps__serialize__4,
                           erlps__deserialize__4,
                           erlps__deserialize_type_and_vsn__1) where
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


erlps__serialize__4 :: ErlangFun
erlps__serialize__4 [type_0, vsn_1, template_2, fields_3] =
  let arg_4 = erlps__tag__1 [type_0]
  in
    BIF.do_remote_fun_call "Aeserialization" "erlps__serialize__4"
      [arg_4, vsn_1, template_2, fields_3]
erlps__serialize__4 [arg_9, arg_10, arg_11, arg_12] =
  EXC.function_clause unit
erlps__serialize__4 args =
  EXC.badarity (ErlangFun 4 erlps__serialize__4) args

erlps__deserialize_type_and_vsn__1 :: ErlangFun
erlps__deserialize_type_and_vsn__1 [binary_0] =
  let
    matchExpr_5 =
      BIF.do_remote_fun_call "Aeserialization"
        "erlps__deserialize_tag_and_vsn__1" [binary_0]
  in
    case matchExpr_5 of
      (ErlangTuple [tag_2, vsn_3, fields_4]) ->
        let tup_el_6 = erlps__rev_tag__1 [tag_2]
        in ErlangTuple [tup_el_6, vsn_3, fields_4]
      _ -> EXC.badmatch matchExpr_5
erlps__deserialize_type_and_vsn__1 [arg_10] =
  EXC.function_clause unit
erlps__deserialize_type_and_vsn__1 args =
  EXC.badarity (ErlangFun 1 erlps__deserialize_type_and_vsn__1)
    args

erlps__deserialize__4 :: ErlangFun
erlps__deserialize__4 [type_0, vsn_1, template_2, binary_3] =
  let arg_5 = erlps__tag__1 [type_0]
  in
    BIF.do_remote_fun_call "Aeserialization" "erlps__deserialize__5"
      [type_0, arg_5, vsn_1, template_2, binary_3]
erlps__deserialize__4 [arg_10, arg_11, arg_12, arg_13] =
  EXC.function_clause unit
erlps__deserialize__4 args =
  EXC.badarity (ErlangFun 4 erlps__deserialize__4) args

erlps__tag__1 :: ErlangFun
erlps__tag__1 [(ErlangAtom "account")] = toErl 10
erlps__tag__1 [(ErlangAtom "signed_tx")] = toErl 11
erlps__tag__1 [(ErlangAtom "spend_tx")] = toErl 12
erlps__tag__1 [(ErlangAtom "oracle")] = toErl 20
erlps__tag__1 [(ErlangAtom "oracle_query")] = toErl 21
erlps__tag__1 [(ErlangAtom "oracle_register_tx")] = toErl 22
erlps__tag__1 [(ErlangAtom "oracle_query_tx")] = toErl 23
erlps__tag__1 [(ErlangAtom "oracle_response_tx")] = toErl 24
erlps__tag__1 [(ErlangAtom "oracle_extend_tx")] = toErl 25
erlps__tag__1 [(ErlangAtom "name")] = toErl 30
erlps__tag__1 [(ErlangAtom "name_commitment")] = toErl 31
erlps__tag__1 [(ErlangAtom "name_claim_tx")] = toErl 32
erlps__tag__1 [(ErlangAtom "name_preclaim_tx")] = toErl 33
erlps__tag__1 [(ErlangAtom "name_update_tx")] = toErl 34
erlps__tag__1 [(ErlangAtom "name_revoke_tx")] = toErl 35
erlps__tag__1 [(ErlangAtom "name_transfer_tx")] = toErl 36
erlps__tag__1 [(ErlangAtom "name_auction")] = toErl 37
erlps__tag__1 [(ErlangAtom "contract")] = toErl 40
erlps__tag__1 [(ErlangAtom "contract_call")] = toErl 41
erlps__tag__1 [(ErlangAtom "contract_create_tx")] = toErl 42
erlps__tag__1 [(ErlangAtom "contract_call_tx")] = toErl 43
erlps__tag__1 [(ErlangAtom "channel_create_tx")] = toErl 50
erlps__tag__1 [(ErlangAtom "channel_deposit_tx")] = toErl 51
erlps__tag__1 [(ErlangAtom "channel_withdraw_tx")] = toErl 52
erlps__tag__1 [(ErlangAtom "channel_force_progress_tx")] =
  toErl 521
erlps__tag__1 [(ErlangAtom "channel_close_mutual_tx")] = toErl 53
erlps__tag__1 [(ErlangAtom "channel_close_solo_tx")] = toErl 54
erlps__tag__1 [(ErlangAtom "channel_slash_tx")] = toErl 55
erlps__tag__1 [(ErlangAtom "channel_settle_tx")] = toErl 56
erlps__tag__1 [(ErlangAtom "channel_offchain_tx")] = toErl 57
erlps__tag__1 [(ErlangAtom "channel_offchain_update_transfer")] =
  toErl 570
erlps__tag__1 [(ErlangAtom "channel_offchain_update_deposit")] =
  toErl 571
erlps__tag__1 [(ErlangAtom "channel_offchain_update_withdraw")] =
  toErl 572
erlps__tag__1 [(ErlangAtom "channel_offchain_update_create_contract")]
  =
  toErl 573
erlps__tag__1 [(ErlangAtom "channel_offchain_update_call_contract")]
  =
  toErl 574
erlps__tag__1 [(ErlangAtom "channel_offchain_update_meta")] =
  toErl 576
erlps__tag__1 [(ErlangAtom "channel_client_reconnect_tx")] =
  toErl 575
erlps__tag__1 [(ErlangAtom "channel")] = toErl 58
erlps__tag__1 [(ErlangAtom "channel_snapshot_solo_tx")] =
  toErl 59
erlps__tag__1 [(ErlangAtom "trees_poi")] = toErl 60
erlps__tag__1 [(ErlangAtom "trees_db")] = toErl 61
erlps__tag__1 [(ErlangAtom "state_trees")] = toErl 62
erlps__tag__1 [(ErlangAtom "mtree")] = toErl 63
erlps__tag__1 [(ErlangAtom "mtree_value")] = toErl 64
erlps__tag__1 [(ErlangAtom "contracts_mtree")] = toErl 621
erlps__tag__1 [(ErlangAtom "calls_mtree")] = toErl 622
erlps__tag__1 [(ErlangAtom "channels_mtree")] = toErl 623
erlps__tag__1 [(ErlangAtom "nameservice_mtree")] = toErl 624
erlps__tag__1 [(ErlangAtom "oracles_mtree")] = toErl 625
erlps__tag__1 [(ErlangAtom "accounts_mtree")] = toErl 626
erlps__tag__1 [(ErlangAtom "compiler_sophia")] = toErl 70
erlps__tag__1 [(ErlangAtom "ga_attach_tx")] = toErl 80
erlps__tag__1 [(ErlangAtom "ga_meta_tx")] = toErl 81
erlps__tag__1 [(ErlangAtom "key_block")] = toErl 100
erlps__tag__1 [(ErlangAtom "micro_block")] = toErl 101
erlps__tag__1 [(ErlangAtom "light_micro_block")] = toErl 102
erlps__tag__1 [(ErlangAtom "pof")] = toErl 200
erlps__tag__1 [arg_0] = EXC.function_clause unit
erlps__tag__1 args =
  EXC.badarity (ErlangFun 1 erlps__tag__1) args

erlps__rev_tag__1 :: ErlangFun
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 10) =
  ErlangAtom "account"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 11) =
  ErlangAtom "signed_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 12) =
  ErlangAtom "spend_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 20) =
  ErlangAtom "oracle"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 21) =
  ErlangAtom "oracle_query"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 22) =
  ErlangAtom "oracle_register_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 23) =
  ErlangAtom "oracle_query_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 24) =
  ErlangAtom "oracle_response_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 25) =
  ErlangAtom "oracle_extend_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 30) =
  ErlangAtom "name"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 31) =
  ErlangAtom "name_commitment"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 32) =
  ErlangAtom "name_claim_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 33) =
  ErlangAtom "name_preclaim_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 34) =
  ErlangAtom "name_update_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 35) =
  ErlangAtom "name_revoke_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 36) =
  ErlangAtom "name_transfer_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 37) =
  ErlangAtom "name_auction"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 40) =
  ErlangAtom "contract"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 41) =
  ErlangAtom "contract_call"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 42) =
  ErlangAtom "contract_create_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 43) =
  ErlangAtom "contract_call_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 50) =
  ErlangAtom "channel_create_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 51) =
  ErlangAtom "channel_deposit_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 52) =
  ErlangAtom "channel_withdraw_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 521) =
  ErlangAtom "channel_force_progress_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 53) =
  ErlangAtom "channel_close_mutual_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 54) =
  ErlangAtom "channel_close_solo_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 55) =
  ErlangAtom "channel_slash_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 56) =
  ErlangAtom "channel_settle_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 57) =
  ErlangAtom "channel_offchain_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 570) =
  ErlangAtom "channel_offchain_update_transfer"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 571) =
  ErlangAtom "channel_offchain_update_deposit"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 572) =
  ErlangAtom "channel_offchain_update_withdraw"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 573) =
  ErlangAtom "channel_offchain_update_create_contract"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 574) =
  ErlangAtom "channel_offchain_update_call_contract"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 576) =
  ErlangAtom "channel_offchain_update_meta"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 575) =
  ErlangAtom "channel_client_reconnect_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 58) =
  ErlangAtom "channel"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 59) =
  ErlangAtom "channel_snapshot_solo_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 60) =
  ErlangAtom "trees_poi"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 61) =
  ErlangAtom "trees_db"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 62) =
  ErlangAtom "state_trees"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 63) =
  ErlangAtom "mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 64) =
  ErlangAtom "mtree_value"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 621) =
  ErlangAtom "contracts_mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 622) =
  ErlangAtom "calls_mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 623) =
  ErlangAtom "channels_mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 624) =
  ErlangAtom "nameservice_mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 625) =
  ErlangAtom "oracles_mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 626) =
  ErlangAtom "accounts_mtree"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 70) =
  ErlangAtom "compiler_sophia"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 80) =
  ErlangAtom "ga_attach_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 81) =
  ErlangAtom "ga_meta_tx"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 100) =
  ErlangAtom "key_block"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 101) =
  ErlangAtom "micro_block"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 102) =
  ErlangAtom "light_micro_block"
erlps__rev_tag__1 [(ErlangInt num_0)]
  | (ErlangInt num_0) == (toErl 200) =
  ErlangAtom "pof"
erlps__rev_tag__1 [arg_1] = EXC.function_clause unit
erlps__rev_tag__1 args =
  EXC.badarity (ErlangFun 1 erlps__rev_tag__1) args