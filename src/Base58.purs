module Base58(erlps__check_base58__1, erlps__binary_to_base58__1,
              erlps__integer_to_base58__1, erlps__base58_to_integer__1,
              erlps__base58_to_binary__1) where
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


erlps__b58char__1 :: ErlangFun
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 0))) =
  (ErlangInt (DBI.fromInt 49))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 1))) =
  (ErlangInt (DBI.fromInt 50))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 2))) =
  (ErlangInt (DBI.fromInt 51))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 3))) =
  (ErlangInt (DBI.fromInt 52))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 4))) =
  (ErlangInt (DBI.fromInt 53))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 5))) =
  (ErlangInt (DBI.fromInt 54))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 6))) =
  (ErlangInt (DBI.fromInt 55))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 7))) =
  (ErlangInt (DBI.fromInt 56))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 8))) =
  (ErlangInt (DBI.fromInt 57))
erlps__b58char__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 9))) =
  (ErlangInt (DBI.fromInt 65))
erlps__b58char__1 args = (erlps__b58char__1__p1 args)

erlps__b58char__1__p1 :: ErlangFun
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 10))) =
  (ErlangInt (DBI.fromInt 66))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 11))) =
  (ErlangInt (DBI.fromInt 67))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 12))) =
  (ErlangInt (DBI.fromInt 68))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 13))) =
  (ErlangInt (DBI.fromInt 69))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 14))) =
  (ErlangInt (DBI.fromInt 70))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 15))) =
  (ErlangInt (DBI.fromInt 71))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 16))) =
  (ErlangInt (DBI.fromInt 72))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 17))) =
  (ErlangInt (DBI.fromInt 74))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 18))) =
  (ErlangInt (DBI.fromInt 75))
erlps__b58char__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 19))) =
  (ErlangInt (DBI.fromInt 76))
erlps__b58char__1__p1 args = (erlps__b58char__1__p2 args)

erlps__b58char__1__p2 :: ErlangFun
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 20))) =
  (ErlangInt (DBI.fromInt 77))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 21))) =
  (ErlangInt (DBI.fromInt 78))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 22))) =
  (ErlangInt (DBI.fromInt 80))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 23))) =
  (ErlangInt (DBI.fromInt 81))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 24))) =
  (ErlangInt (DBI.fromInt 82))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 25))) =
  (ErlangInt (DBI.fromInt 83))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 26))) =
  (ErlangInt (DBI.fromInt 84))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 27))) =
  (ErlangInt (DBI.fromInt 85))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 28))) =
  (ErlangInt (DBI.fromInt 86))
erlps__b58char__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 29))) =
  (ErlangInt (DBI.fromInt 87))
erlps__b58char__1__p2 args = (erlps__b58char__1__p3 args)

erlps__b58char__1__p3 :: ErlangFun
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 30))) =
  (ErlangInt (DBI.fromInt 88))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 31))) =
  (ErlangInt (DBI.fromInt 89))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 32))) =
  (ErlangInt (DBI.fromInt 90))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 33))) =
  (ErlangInt (DBI.fromInt 97))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 34))) =
  (ErlangInt (DBI.fromInt 98))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 35))) =
  (ErlangInt (DBI.fromInt 99))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 36))) =
  (ErlangInt (DBI.fromInt 100))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 37))) =
  (ErlangInt (DBI.fromInt 101))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 38))) =
  (ErlangInt (DBI.fromInt 102))
erlps__b58char__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 39))) =
  (ErlangInt (DBI.fromInt 103))
erlps__b58char__1__p3 args = (erlps__b58char__1__p4 args)

erlps__b58char__1__p4 :: ErlangFun
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 40))) =
  (ErlangInt (DBI.fromInt 104))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 41))) =
  (ErlangInt (DBI.fromInt 105))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 42))) =
  (ErlangInt (DBI.fromInt 106))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 43))) =
  (ErlangInt (DBI.fromInt 107))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 44))) =
  (ErlangInt (DBI.fromInt 109))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 45))) =
  (ErlangInt (DBI.fromInt 110))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 46))) =
  (ErlangInt (DBI.fromInt 111))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 47))) =
  (ErlangInt (DBI.fromInt 112))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 48))) =
  (ErlangInt (DBI.fromInt 113))
erlps__b58char__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 49))) =
  (ErlangInt (DBI.fromInt 114))
erlps__b58char__1__p4 args = (erlps__b58char__1__p5 args)

erlps__b58char__1__p5 :: ErlangFun
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 50))) =
  (ErlangInt (DBI.fromInt 115))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 51))) =
  (ErlangInt (DBI.fromInt 116))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 52))) =
  (ErlangInt (DBI.fromInt 117))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 53))) =
  (ErlangInt (DBI.fromInt 118))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 54))) =
  (ErlangInt (DBI.fromInt 119))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 55))) =
  (ErlangInt (DBI.fromInt 120))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 56))) =
  (ErlangInt (DBI.fromInt 121))
erlps__b58char__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 57))) =
  (ErlangInt (DBI.fromInt 122))
erlps__b58char__1__p5 [_] = (ErlangAtom "error")
erlps__b58char__1__p5 [arg_0] = (EXC.function_clause unit)
erlps__b58char__1__p5 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__charb58__1 :: ErlangFun
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 49))) =
  (ErlangInt (DBI.fromInt 0))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 50))) =
  (ErlangInt (DBI.fromInt 1))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 51))) =
  (ErlangInt (DBI.fromInt 2))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 52))) =
  (ErlangInt (DBI.fromInt 3))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 53))) =
  (ErlangInt (DBI.fromInt 4))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 54))) =
  (ErlangInt (DBI.fromInt 5))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 55))) =
  (ErlangInt (DBI.fromInt 6))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 56))) =
  (ErlangInt (DBI.fromInt 7))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 57))) =
  (ErlangInt (DBI.fromInt 8))
erlps__charb58__1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 65))) =
  (ErlangInt (DBI.fromInt 9))
erlps__charb58__1 args = (erlps__charb58__1__p1 args)

erlps__charb58__1__p1 :: ErlangFun
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 66))) =
  (ErlangInt (DBI.fromInt 10))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 67))) =
  (ErlangInt (DBI.fromInt 11))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 68))) =
  (ErlangInt (DBI.fromInt 12))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 69))) =
  (ErlangInt (DBI.fromInt 13))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 70))) =
  (ErlangInt (DBI.fromInt 14))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 71))) =
  (ErlangInt (DBI.fromInt 15))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 72))) =
  (ErlangInt (DBI.fromInt 16))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 74))) =
  (ErlangInt (DBI.fromInt 17))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 75))) =
  (ErlangInt (DBI.fromInt 18))
erlps__charb58__1__p1 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 76))) =
  (ErlangInt (DBI.fromInt 19))
erlps__charb58__1__p1 args = (erlps__charb58__1__p2 args)

erlps__charb58__1__p2 :: ErlangFun
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 77))) =
  (ErlangInt (DBI.fromInt 20))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 78))) =
  (ErlangInt (DBI.fromInt 21))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 80))) =
  (ErlangInt (DBI.fromInt 22))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 81))) =
  (ErlangInt (DBI.fromInt 23))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 82))) =
  (ErlangInt (DBI.fromInt 24))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 83))) =
  (ErlangInt (DBI.fromInt 25))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 84))) =
  (ErlangInt (DBI.fromInt 26))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 85))) =
  (ErlangInt (DBI.fromInt 27))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 86))) =
  (ErlangInt (DBI.fromInt 28))
erlps__charb58__1__p2 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 87))) =
  (ErlangInt (DBI.fromInt 29))
erlps__charb58__1__p2 args = (erlps__charb58__1__p3 args)

erlps__charb58__1__p3 :: ErlangFun
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 88))) =
  (ErlangInt (DBI.fromInt 30))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 89))) =
  (ErlangInt (DBI.fromInt 31))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 90))) =
  (ErlangInt (DBI.fromInt 32))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 97))) =
  (ErlangInt (DBI.fromInt 33))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 98))) =
  (ErlangInt (DBI.fromInt 34))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 99))) =
  (ErlangInt (DBI.fromInt 35))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 100))) =
  (ErlangInt (DBI.fromInt 36))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 101))) =
  (ErlangInt (DBI.fromInt 37))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 102))) =
  (ErlangInt (DBI.fromInt 38))
erlps__charb58__1__p3 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 103))) =
  (ErlangInt (DBI.fromInt 39))
erlps__charb58__1__p3 args = (erlps__charb58__1__p4 args)

erlps__charb58__1__p4 :: ErlangFun
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 104))) =
  (ErlangInt (DBI.fromInt 40))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 105))) =
  (ErlangInt (DBI.fromInt 41))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 106))) =
  (ErlangInt (DBI.fromInt 42))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 107))) =
  (ErlangInt (DBI.fromInt 43))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 109))) =
  (ErlangInt (DBI.fromInt 44))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 110))) =
  (ErlangInt (DBI.fromInt 45))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 111))) =
  (ErlangInt (DBI.fromInt 46))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 112))) =
  (ErlangInt (DBI.fromInt 47))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 113))) =
  (ErlangInt (DBI.fromInt 48))
erlps__charb58__1__p4 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 114))) =
  (ErlangInt (DBI.fromInt 49))
erlps__charb58__1__p4 args = (erlps__charb58__1__p5 args)

erlps__charb58__1__p5 :: ErlangFun
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 115))) =
  (ErlangInt (DBI.fromInt 50))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 116))) =
  (ErlangInt (DBI.fromInt 51))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 117))) =
  (ErlangInt (DBI.fromInt 52))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 118))) =
  (ErlangInt (DBI.fromInt 53))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 119))) =
  (ErlangInt (DBI.fromInt 54))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 120))) =
  (ErlangInt (DBI.fromInt 55))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 121))) =
  (ErlangInt (DBI.fromInt 56))
erlps__charb58__1__p5 [(ErlangInt num_0)]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 122))) =
  (ErlangInt (DBI.fromInt 57))
erlps__charb58__1__p5 [_] = (ErlangAtom "error")
erlps__charb58__1__p5 [arg_0] = (EXC.function_clause unit)
erlps__charb58__1__p5 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__check_base58__1 :: ErlangFun
erlps__check_base58__1 [base58_0] =
  let   
    arg_1 =
      (ErlangFun 1
         let
           lambda_2 [c_4] =
             (BIF.erlang__op_exactNeq [c_4, (ErlangAtom "error")])
           lambda_2 [arg_3] = (EXC.function_clause unit)
           lambda_2 args = (EXC.badarity (ErlangFun 1 lambda_2) args)
         in lambda_2)
  in let
    arg_8 =
      (ErlangFun 1
         let
           lambda_9 [c_11] = (erlps__charb58__1 [c_11])
           lambda_9 [arg_10] = (EXC.function_clause unit)
           lambda_9 args = (EXC.badarity (ErlangFun 1 lambda_9) args)
         in lambda_9)
  in let
    arg_7 =
      (BIF.do_remote_fun_call "Lists" "erlps__map__2"
         [arg_8, base58_0])
  in
    (BIF.do_remote_fun_call "Lists" "erlps__all__2" [arg_1, arg_7])
erlps__check_base58__1 [arg_14] = (EXC.function_clause unit)
erlps__check_base58__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__integer_to_base58__1 :: ErlangFun
erlps__integer_to_base58__1 [integer_0] =
  (erlps__integer_to_base58__2 [integer_0, ErlangEmptyList])
erlps__integer_to_base58__1 [arg_3] = (EXC.function_clause unit)
erlps__integer_to_base58__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__integer_to_base58__2 :: ErlangFun
erlps__integer_to_base58__2 [(ErlangInt num_0), acc_1]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 0))) =
  acc_1
erlps__integer_to_base58__2 [integer_0, acc_1] =
  let   
    quot_4 =
      (BIF.erlang__op_div_strict
         [integer_0, (ErlangInt (DBI.fromInt 58))])
  in let
    rem_7 =
      (BIF.erlang__op_rem_strict
         [integer_0, (ErlangInt (DBI.fromInt 58))])
  in let head_10 = (erlps__b58char__1 [rem_7])
  in
    (erlps__integer_to_base58__2
       [quot_4, (ErlangCons head_10 acc_1)])
erlps__integer_to_base58__2 [arg_13, arg_14] =
  (EXC.function_clause unit)
erlps__integer_to_base58__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__base58_to_integer__2 :: ErlangFun
erlps__base58_to_integer__2 [c_0, (ErlangEmptyList)] = c_0
erlps__base58_to_integer__2 [c_0, (ErlangCons x_1 xs_2)] =
  let   
    lop_4 = (BIF.erlang__op_mult [c_0, (ErlangInt (DBI.fromInt 58))])
  in let rop_7 = (erlps__charb58__1 [x_1])
  in let arg_3 = (BIF.erlang__op_plus [lop_4, rop_7])
  in (erlps__base58_to_integer__2 [arg_3, xs_2])
erlps__base58_to_integer__2 [arg_10, arg_11] =
  (EXC.function_clause unit)
erlps__base58_to_integer__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__base58_to_integer__1 :: ErlangFun
erlps__base58_to_integer__1 [(ErlangEmptyList)] =
  (ErlangAtom "error")
erlps__base58_to_integer__1 [(ErlangCons char_0 (ErlangEmptyList))]
  =
  (erlps__charb58__1 [char_0])
erlps__base58_to_integer__1 [(ErlangCons char_0 str_1)] =
  let arg_2 = (erlps__charb58__1 [char_0])
  in (erlps__base58_to_integer__2 [arg_2, str_1])
erlps__base58_to_integer__1 [arg_5] = (EXC.function_clause unit)
erlps__base58_to_integer__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__base58_to_binary__1 :: ErlangFun
erlps__base58_to_binary__1 [base58_0] =
  let    case_1 = (erlps__base58_to_integer__1 [base58_0])
  in let
    bin_6 =
      case case_1 of
        (ErlangInt num_3) | ((ErlangInt num_3) ==
                               (ErlangInt (DBI.fromInt 0))) ->
          (ErlangBinary (BIN.concat []))
        n_4 -> (BIF.binary__encode_unsigned__1 [n_4])
        something_else -> (EXC.case_clause something_else)
  in (erlps__zeroPad__2 [base58_0, bin_6])
erlps__base58_to_binary__1 [arg_9] = (EXC.function_clause unit)
erlps__base58_to_binary__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__binary_to_base58__1 :: ErlangFun
erlps__binary_to_base58__1 [binary_0]
  | ((ErlangAtom "true") ==
       (falsifyErrors (\ _ -> (BIF.erlang__is_binary__1 [binary_0])))) =
  let   
    arg_2 =
      (BIF.do_remote_fun_call "Binary" "erlps__decode_unsigned__1"
         [binary_0])
  in let case_1 = (erlps__integer_to_base58__1 [arg_2])
  in
    case case_1 of
      (ErlangAtom "error") -> (ErlangAtom "error")
      base58_4 -> (erlps__binaryPad__2 [binary_0, base58_4])
      something_else -> (EXC.case_clause something_else)
erlps__binary_to_base58__1 [arg_8] = (EXC.function_clause unit)
erlps__binary_to_base58__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__binaryPad__2 :: ErlangFun
erlps__binaryPad__2 [(ErlangBinary bin_c_0), base58_7]
  | size_1 <- ((DBI.fromInt 8))
  , (BIN.Ok (ErlangInt num_3) bin_2) <-
      ((BIN.chop_int bin_c_0 size_1 1 BIN.Big BIN.Unsigned))
  , ((ErlangInt num_3) == (ErlangInt (DBI.fromInt 0)))
  , (ErlangInt size_4) <- ((BIN.size bin_2))
  , (BIN.Ok rest_6 bin_5) <- ((BIN.chop_bin bin_2 size_4 8))
  , (BIN.empty bin_5) =
  let    lop_10 = (make_string "1")
  in let arg_9 = (BIF.erlang__op_append [lop_10, base58_7])
  in (erlps__binaryPad__2 [rest_6, arg_9])
erlps__binaryPad__2 [_, base58_0] = base58_0
erlps__binaryPad__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__binaryPad__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__zeroPad__2 :: ErlangFun
erlps__zeroPad__2 [(ErlangCons (ErlangInt num_0) rest_1),
                   base58_2]
  | ((ErlangInt num_0) == (ErlangInt (DBI.fromInt 49))) =
  let
    arg_4 =
      (ErlangBinary
         (BIN.concat
            [(BIN.from_int (ErlangInt (DBI.fromInt 0))
                (ErlangInt (DBI.fromInt 8)) 1 BIN.Big),
             (BIN.format_bin base58_2 (BIN.packed_size base58_2) 8)]))
  in (erlps__zeroPad__2 [rest_1, arg_4])
erlps__zeroPad__2 [_, base58_0] = base58_0
erlps__zeroPad__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__zeroPad__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)