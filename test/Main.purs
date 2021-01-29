module Test.Main where

import Prelude

import Effect.Aff.AVar as AVar
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Unsafe
import Effect.Ref as Ref
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff hiding (error)
import Effect.Exception(catchException)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, expectError, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.String.CodePoints as StrCP
import Data.String as Str
import Unsafe.Coerce

import Data.Time.Duration
import Data.Lazy
import Data.Either
import Data.Tuple as T
import Data.Array as A
import Data.Maybe as M
import Data.Traversable
import Partial.Unsafe
import Erlang.Type
import Erlang.Helpers as H
import Erlang.Exception
import Erlang.TestUtil
import Erlang.Builtins as BIF
import Data.BigInt as DBI
import Erlang.Invoke

import Aeser.Api.Encoder.Tests
import Aeser.Rlp.Tests
import Aeser.Chain.Objects.Tests

assertOk f args = do
  r <- exec f args
  case r of
    ErlangTuple [ErlangAtom "ok", _] -> pure unit
    _ -> fail $ "Not ok: " <> show r

main :: Effect Unit
main = unsafePartial $
    launchAff_ $ runSpec [consoleReporter] do
      describe "AeserApiEncoderTests" do
        for_ (M.fromJust $ fromErl $ erlps__encode_decode_test___0 [] :: Array ErlangTerm) $
          \(ErlangTuple [ename, ErlangFun 0 f]) -> do
            it (M.fromJust $ fromErl ename) do
              testExecOk ok f []
      describe "AeserRlpTests" do
        it "erlps__rlp_one_byte_test__0" do
            assertOk erlps__rlp_one_byte_test__0 []
        it "erlps__rlp_another_one_byte_test__0" do
            assertOk erlps__rlp_another_one_byte_test__0 []
        it "erlps__rlp_zero_bytes_test__0" do
            assertOk erlps__rlp_zero_bytes_test__0 []
        it "erlps__rlp_two_bytes_test__0" do
            assertOk erlps__rlp_two_bytes_test__0 []
        it "erlps__rlp_one_byte_size_bytes_test__0" do
            assertOk erlps__rlp_one_byte_size_bytes_test__0 []
        it "erlps__rlp_tagged_size_one_byte_bytes_test__0" do
            assertOk erlps__rlp_tagged_size_one_byte_bytes_test__0 []
        it "erlps__rlp_tagged_size_two_bytes_bytes_test__0" do
            assertOk erlps__rlp_tagged_size_two_bytes_bytes_test__0 []
        it "erlps__rlp_zero_bytes_list_test__0" do
            assertOk erlps__rlp_zero_bytes_list_test__0 []
        it "erlps__rlp_one_byte_list_test__0" do
            assertOk erlps__rlp_one_byte_list_test__0 []
        it "erlps__rlp_byte_array_list_test__0" do
            assertOk erlps__rlp_byte_array_list_test__0 []
        it "erlps__rlp_byte_array_tagged_size_one_byte_list_test__0" do
            assertOk erlps__rlp_byte_array_tagged_size_one_byte_list_test__0 []
        it "erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0" do
            assertOk erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0 []
        it "erlps__illegal_size_encoding_list_test__0" do
            assertOk erlps__illegal_size_encoding_list_test__0 []
        it "erlps__illegal_size_encoding_byte_array_test__0" do
            assertOk erlps__illegal_size_encoding_byte_array_test__0 []
      describe "AeserChainObjectsTests" do
        it "erlps__basic_test__0" do
            assertOk erlps__basic_test__0 []
        it "erlps__basic_fail_test__0" do
            assertOk erlps__basic_fail_test__0 []
        it "erlps__list_test__0" do
            assertOk erlps__list_test__0 []
        it "erlps__list_fail_test__0" do
            assertOk erlps__list_fail_test__0 []
        it "erlps__deep_list_test__0" do
            assertOk erlps__deep_list_test__0 []
        it "erlps__deep_list_fail_test__0" do
            assertOk erlps__deep_list_fail_test__0 []
        it "erlps__array_test__0" do
            assertOk erlps__array_test__0 []
        it "erlps__array_fail_test__0" do
            assertOk erlps__array_fail_test__0 []
        it "erlps__deep_array_test__0" do
            assertOk erlps__deep_array_test__0 []
        it "erlps__deep_array_fail_test__0" do
            assertOk erlps__deep_array_fail_test__0 []
        it "erlps__tag_fail_test__0" do
            assertOk erlps__tag_fail_test__0 []
        it "erlps__vsn_fail_test__0" do
            assertOk erlps__vsn_fail_test__0 []
