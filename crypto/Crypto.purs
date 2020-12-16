module Crypto where

import Data.BigInt as DBI
import Erlang.Exception as EXC
import Erlang.Type
import Erlang.Binary as BIN
import Erlang.Helpers as H
import Data.Maybe as DM
import Prelude
import Partial.Unsafe
import Node.Buffer
import Unsafe.Coerce

foreign import sha256Impl :: String -> String

erlps__hash__2 :: ErlangFun
erlps__hash__2 [ErlangAtom "sha256", ErlangBinary buf]
  = unsafePartial
    $ ErlangBinary
    $ BIN.fromFoldable
    $ map (\(ErlangInt bi) -> DM.fromJust $ H.bigIntToInt bi)
    $ DM.fromJust
    $ erlangListToList
    $ H.make_string
    $ sha256Impl
    $ DM.fromJust
    $ H.erlangListToString
    $ BIN.to_erlang_list
    $ buf
erlps__hash__2 _ = EXC.badarg unit
