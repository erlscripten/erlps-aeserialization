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
import Test.Spec.Assertions (shouldEqual, expectError)
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
import Erlang.Builtins as BIF
import Data.BigInt as DBI
import Erlang.Invoke

import Aeser.Api.Encoder.Tests
import Aeser.Rlp.Tests
import Aeser.Chain.Objects.Tests

-- BEWARE - HERE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
exec_may_throw_aff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw_aff fun args =
    let
      t = defer $ (\_ -> run_erlang fun args)
      f = defer $ (\_ -> ErlangAtom "error")
    in do
      v <- liftEffect (catchException (\_ -> pure f
                                      ) (pure t))
      pure $ force v

wololo_term :: Error -> ErlangTerm
wololo_term res = unsafeCoerce res

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce res

print_err (Right r) = show r
print_err (Left e) =
    case show e of
        "[object Object]" ->
            case (wololo_term e) of
                ErlangTuple [a,b,stack] ->
                    let
                        m1 = show a
                        m2 = show b
                        m3 = show stack
                    in
                        "[" <> m1 <> ", " <> m2 <> ", " <> m3 <> "]"
                r ->
                    show r
        r ->
            r

exec_may_throw :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw fun args = do
    res <- attempt $ exec_may_throw_aff fun args
    liftEffect $ log $ print_err res -- Uncomment for logs :)
    case res of
        Left _ -> pure make_err
        Right r -> pure $ make_ok r

lift_aff_to_erlang_process :: forall a. (Unit -> Aff a) -> Aff (T.Tuple ErlangTerm a)
lift_aff_to_erlang_process calc = do
        -- ONLY TOUCH THIS IF YOU KNOW WHAT YOU ARE DOING!!!!!
        -- THIS IS A DIRTY HACK TO "lift" an calculation in the Aff monad to an ErlangProcess from the GLOBAL scope
        res_channel <- AVar.empty
        pid_channel <- AVar.empty
        _ <- forkAff do
            packed_pid <- exec_may_throw BIF.erlang__spawn__1 [(
                ErlangFun 0 (\ _ -> let -- TODO: Fixme - the calculation should yield to the scheduler and only then we may launch the avar. We need a jump to FFI here :(
                    a = unsafePerformEffect $ launchAff_ (
                        do
                            res <- calc unit
                            AVar.put res res_channel
                        )
                    in
                       ErlangInt (DBI.fromInt 1)))]
            -- At this point we never yielded so the process MUST be alive
            pid <- unpack_ok packed_pid
            AVar.put pid pid_channel

        pid <- AVar.take pid_channel
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "true") `shouldEqualOk` packed_is_alive

        res <- AVar.take res_channel

        delay (Milliseconds 1.0) -- force a context switch to cleanup the process :P
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "false") `shouldEqualOk` packed_is_alive
        pure $ T.Tuple pid res

unpack_ok :: ErlangTerm -> Aff ErlangTerm
unpack_ok (ErlangTuple [ErlangAtom "ok", term]) = pure term
unpack_ok e = do
    ErlangEmptyList `shouldEqual` e
    pure ErlangEmptyList

assert_ok :: ErlangTerm -> Aff Unit
assert_ok (ErlangTuple [ErlangAtom "ok", _]) = pure unit
assert_ok e = do
    ErlangEmptyList `shouldEqual` e
    pure unit

make_ok term = ErlangTuple [ErlangAtom "ok", term]
make_err = ErlangAtom "error"
mkInt :: Int -> ErlangTerm
mkInt = DBI.fromInt >>> ErlangInt

shouldEqualOk a b = make_ok a `shouldEqual` b

main :: Effect Unit
main = unsafePartial $
    launchAff_ $ runSpec [consoleReporter] do
      describe "AeserApiEncoderTests" do
        for_ (M.fromJust $ erlangListToList $ erlps__encode_decode_test___0 []) $
          \(ErlangTuple [ename, ErlangFun 0 f]) -> do
            it (M.fromJust $ H.erlangListToString ename) do
              r <- exec_may_throw f []
              ErlangAtom "ok" `shouldEqualOk` r
      describe "AeserRlpTests" do
        it "erlps__rlp_one_byte_test__0" do
            r <- exec_may_throw erlps__rlp_one_byte_test__0 []
            assert_ok r
        it "erlps__rlp_another_one_byte_test__0" do
            r <- exec_may_throw erlps__rlp_another_one_byte_test__0 []
            assert_ok r
        it "erlps__rlp_zero_bytes_test__0" do
            r <- exec_may_throw erlps__rlp_zero_bytes_test__0 []
            assert_ok r
        it "erlps__rlp_two_bytes_test__0" do
            r <- exec_may_throw erlps__rlp_two_bytes_test__0 []
            assert_ok r
        it "erlps__rlp_one_byte_size_bytes_test__0" do
            r <- exec_may_throw erlps__rlp_one_byte_size_bytes_test__0 []
            assert_ok r
        it "erlps__rlp_tagged_size_one_byte_bytes_test__0" do
            r <- exec_may_throw erlps__rlp_tagged_size_one_byte_bytes_test__0 []
            assert_ok r
        it "erlps__rlp_tagged_size_two_bytes_bytes_test__0" do
            r <- exec_may_throw erlps__rlp_tagged_size_two_bytes_bytes_test__0 []
            assert_ok r
        it "erlps__rlp_zero_bytes_list_test__0" do
            r <- exec_may_throw erlps__rlp_zero_bytes_list_test__0 []
            assert_ok r
        it "erlps__rlp_one_byte_list_test__0" do
            r <- exec_may_throw erlps__rlp_one_byte_list_test__0 []
            assert_ok r
        it "erlps__rlp_byte_array_list_test__0" do
            r <- exec_may_throw erlps__rlp_byte_array_list_test__0 []
            assert_ok r
        it "erlps__rlp_byte_array_tagged_size_one_byte_list_test__0" do
            r <- exec_may_throw erlps__rlp_byte_array_tagged_size_one_byte_list_test__0 []
            assert_ok r
        it "erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0" do
            r <- exec_may_throw erlps__rlp_byte_array_tagged_size_two_bytes_list_test__0 []
            assert_ok r
        it "erlps__illegal_size_encoding_list_test__0" do
            r <- exec_may_throw erlps__illegal_size_encoding_list_test__0 []
            assert_ok r
        it "erlps__illegal_size_encoding_byte_array_test__0" do
            r <- exec_may_throw erlps__illegal_size_encoding_byte_array_test__0 []
            assert_ok r
      describe "AeserChainObjectsTests" do
        it "erlps__basic_test__0" do
            r <- exec_may_throw erlps__basic_test__0 []
            assert_ok r
        it "erlps__basic_fail_test__0" do
            r <- exec_may_throw erlps__basic_fail_test__0 []
            assert_ok r
        it "erlps__list_test__0" do
            r <- exec_may_throw erlps__list_test__0 []
            assert_ok r
        it "erlps__list_fail_test__0" do
            r <- exec_may_throw erlps__list_fail_test__0 []
            assert_ok r
        it "erlps__deep_list_test__0" do
            r <- exec_may_throw erlps__deep_list_test__0 []
            assert_ok r
        it "erlps__deep_list_fail_test__0" do
            r <- exec_may_throw erlps__deep_list_fail_test__0 []
            assert_ok r
        it "erlps__array_test__0" do
            r <- exec_may_throw erlps__array_test__0 []
            assert_ok r
        it "erlps__array_fail_test__0" do
            r <- exec_may_throw erlps__array_fail_test__0 []
            assert_ok r
        it "erlps__deep_array_test__0" do
            r <- exec_may_throw erlps__deep_array_test__0 []
            assert_ok r
        it "erlps__deep_array_fail_test__0" do
            r <- exec_may_throw erlps__deep_array_fail_test__0 []
            assert_ok r
        it "erlps__tag_fail_test__0" do
            r <- exec_may_throw erlps__tag_fail_test__0 []
            assert_ok r
        it "erlps__vsn_fail_test__0" do
            r <- exec_may_throw erlps__vsn_fail_test__0 []
            assert_ok r
