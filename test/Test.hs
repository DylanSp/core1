import qualified Data.Map as Map
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Eval
import Syntax

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [properties, units]

-- property-based testing

properties :: TestTree
properties = testGroup "Properties" [ identity
                                    , kConst
                                    , skki
                                    , ifTrue
                                    , ifFalse
                                    , addOp
                                    , subOp
                                    , mulOp
                                    , eqOp ]

identity = testProperty "I combinator is identity" propIdentity
kConst = testProperty "K combinator is const" propKConst
skki = testProperty "SKK == I" propSKKI
ifTrue = testProperty "if true a b == a" propIfTrue
ifFalse = testProperty "if false a b == b" propIfFalse
addOp = testProperty "Add a b == a + b" propAddOp
subOp = testProperty "Subtract a b == a - b" propSubOp
mulOp = testProperty "Multiply a b == a * b" propMulOp
eqOp = testProperty "Equals a b == (a == b)" propEqOp

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

-- if true a b == a
propIfTrue :: HH.Property
propIfTrue = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    let vm = VInt m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval Map.empty (If (Lit (LBool True)) lm ln) === vm

-- if false a b == b
propIfFalse :: HH.Property
propIfFalse = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    let vn = VInt n
    eval Map.empty (If (Lit (LBool False)) lm ln) === vn

propAddOp :: HH.Property
propAddOp = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval Map.empty (Op Add lm ln) === VInt (m + n)

propSubOp :: HH.Property
propSubOp = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval Map.empty (Op Subtract lm ln) === VInt (m - n)

propMulOp :: HH.Property
propMulOp = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval Map.empty (Op Multiply lm ln) === VInt (m * n)

propEqOp :: HH.Property
propEqOp = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval Map.empty (Op Equals lm ln) === VBool (m == n)

-- unit testing

units :: TestTree
units = testGroup "Unit Tests" [letExample]

letExample :: TestTree
letExample = testCase "let x = 3 in x evaluates to 3" $ do
    let letExpression = Let "x" (Lit (LInt 3)) (Var "x")
    eval Map.empty letExpression @?= VInt 3