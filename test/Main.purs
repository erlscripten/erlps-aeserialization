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
import Aeser.Contract.Code.Tests
import Aeser.Contract.Code

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
      describe "AeserContractCodeTests" do
        it "vsn_1_test" do
            assertOk erlps__vsn_1_test__0 []
        it "vsn_1_test bis" do
            let expected = bin [244,70,1,160,48,58,125,237,188,44,120,213,52,155,92,4,213,8,157,236,198,161,240,9,117,91,60,167,64,44,67,82,145,174,238,243,197,196,128,128,128,128,138,68,85,77,77,89,32,67,79,68,69]
            a <- exec erlps__vsn_1_test__0 []
            b <- unpackOk a
            testExecOk expected erlps__serialize__2 [b, toErl 1]
        it "vsn_2_test" do
            assertOk erlps__vsn_2_test__0 []
        it "vsn_2_test bis" do
            let expected = bin [222,70,2,132,1,2,3,4,197,196,128,128,128,128,138,68,85,77,77,89,32,67,79,68,69,133,51,46,49,46,52]
            a <- exec erlps__vsn_2_test__0 []
            b <- unpackOk a
            testExecOk expected erlps__serialize__2 [b, toErl 2]
        it "vsn_3_test" do
            assertOk erlps__vsn_3_test__0 []
        it "vsn_3_test bis" do
            let expected = bin [248,60,70,3,160,48,58,125,237,188,44,120,213,52,155,92,4,213,8,157,236,198,161,240,9,117,91,60,167,64,44,67,82,145,174,238,243,198,197,128,128,0,128,128,138,68,85,77,77,89,32,67,79,68,69,133,51,46,49,46,52,1]
            a <- exec erlps__vsn_3_test__0 []
            b <- unpackOk a
            testExecOk expected erlps__serialize__2 [b, toErl 3]
