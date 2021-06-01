module Tests.Example.Project where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Prelude ((.|.), (.&.), resize)
import Example.Project (plus, opParse, aluOpParse)

prop_plusIsCommutative :: H.Property
prop_plusIsCommutative = H.property $ do
  a <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  b <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  plus a b === plus b a

prop_opParseEqualsAluOpParse :: H.Property
prop_opParseEqualsAluOpParse = H.property $ do
    aluOp <- H.forAll (Gen.integral (Range.linear minBound maxBound))
    let op = 0x6000 .|. 0x1FFF .&. resize aluOp
    aluOpParse aluOp === opParse op

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
