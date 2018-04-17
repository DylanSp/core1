import qualified Data.Map as Map
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Eval
import Syntax

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [identity, kConst, skki]

identity = testProperty "I combinator is identity" propIdentity
kConst = testProperty "K combinator is const" propKConst
skki = testProperty "SKK = I" propSKKI

-- generators

genAnyInt :: HH.Gen Integer
genAnyInt = Gen.integral (Range.linear (-10000) 10000)

-- lambda calculus combinators
iComb :: CoreExpr
iComb = Lambda "x" (Var "x")

kComb :: CoreExpr
kComb = Lambda "x" (Lambda "y" (Var "x"))

sComb :: CoreExpr
sComb = Lambda "f" (Lambda "g" (Lambda "x"  (Apply (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "x")))))

-- tests

-- I combinator should be the identity
propIdentity :: HH.Property
propIdentity = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    let vn = VInt n
    eval (Map.empty) (Apply iComb ln) === vn

-- K combinator should be the const function
propKConst :: HH.Property
propKConst = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    let vm = VInt m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval (Map.empty) (Apply (Apply kComb lm) ln) === vm

-- SKK should be equivalent to I
propSKKI :: HH.Property
propSKKI = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    let vn = VInt n
    eval (Map.empty) (Apply (Apply (Apply sComb kComb) kComb) ln) === vn
