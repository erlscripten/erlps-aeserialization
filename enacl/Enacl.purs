module Enacl where

import Erlang.Exception as EXC
import Erlang.Type
import Erlang.Helpers as H
import Data.Maybe as DM
import Prelude
import Node.Buffer

foreign import generichashImpl :: Int -> Buffer -> Buffer

erlps__generichash__2 [ErlangInt bi, ErlangBinary bin]
 | DM.Just i <- H.bigIntToInt bi
  = ErlangBinary (generichashImpl i bin)
erlps__generichash__2 _ = EXC.badarg unit
