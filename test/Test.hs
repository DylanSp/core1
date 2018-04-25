import qualified Data.Map as Map
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Eval
import Syntax
import Type

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [properties, units]

-- property-based testing

properties :: TestTree
properties = testGroup "Properties" [ combinators, evaluation, typechecking ]

combinators :: TestTree
combinators = testGroup "Lamba calculus combinators" [identity, kConst, skki]

evaluation :: TestTree
evaluation = testGroup "Basic evaluation" [ifTrue, ifFalse, addOp, subOp, mulOp, eqOp]

typechecking :: TestTree
typechecking = testGroup "Type checking" [ intType
                                         , boolType
                                         , appliedFuncType
                                         , arithOpType
                                         , mismatchedArithOpLeft 
                                         , mismatchedArithOpRight
                                         ]

identity = expectFail $ testProperty "I combinator is identity" propIdentity
kConst = expectFail $ testProperty "K combinator is const" propKConst
skki = expectFail $ testProperty "SKK == I" propSKKI
ifTrue = testProperty "if true a b == a" propIfTrue
ifFalse = testProperty "if false a b == b" propIfFalse
addOp = testProperty "Add a b == a + b" propAddOp
subOp = testProperty "Subtract a b == a - b" propSubOp
mulOp = testProperty "Multiply a b == a * b" propMulOp
eqOp = testProperty "Equals a b == (a == b)" propEqOp
intType = testProperty "typeof (any integer) == TInt" propIntType
boolType = testProperty "typeof (any boolean) == TBool" propBoolType
appliedFuncType = testProperty "typeof (\\x : Int -> x) (any integer) == TInt" propAppliedFunc
arithOpType = testProperty "typeof (any Add/Subtract/Multiply) (any int) (any int) == TInt" propArithOp
mismatchedArithOpLeft = testProperty "typeof (Add/Subtract/Multiply) (any bool) (any int) == Mismatch TBool TInt" propArithMismatchLeft
mismatchedArithOpRight = testProperty "typeof (Add/Subtract/Multiply) (any int) (any bool) == Mismatch TInt TBool" propArithMismatchRight

-- generators

genAnyInt :: HH.Gen Integer
genAnyInt = Gen.integral (Range.linear (-10000) 10000)

genArithOp :: HH.Gen BinOp
genArithOp = Gen.element [Add, Subtract, Multiply]

-- lambda calculus combinators
iComb :: CoreExpr
--iComb = Lambda "x" (Var "x")
iComb = undefined

kComb :: CoreExpr
--kComb = Lambda "x" (Lambda "y" (Var "x"))
kComb = undefined

sComb :: CoreExpr
--sComb = Lambda "f" (Lambda "g" (Lambda "x"  (Apply (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "x")))))
sComb = undefined

-- tests

propIdentity :: HH.Property
propIdentity = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    let vn = VInt n
    eval (Map.empty) (Apply iComb ln) === vn

propKConst :: HH.Property
propKConst = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    let vm = VInt m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval (Map.empty) (Apply (Apply kComb lm) ln) === vm

propSKKI :: HH.Property
propSKKI = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    let vn = VInt n
    eval (Map.empty) (Apply (Apply (Apply sComb kComb) kComb) ln) === vn

propIfTrue :: HH.Property
propIfTrue = HH.property $ do
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    let vm = VInt m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    eval Map.empty (If (Lit (LBool True)) lm ln) === vm

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

propIntType :: HH.Property
propIntType = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    runTypecheck Map.empty (typeCheck ln) === Right TInt

propBoolType :: HH.Property
propBoolType = HH.property $ do
    b <- HH.forAll Gen.bool
    let lb = Lit . LBool $ b
    runTypecheck Map.empty (typeCheck lb) === Right TBool

propAppliedFunc :: HH.Property
propAppliedFunc = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    runTypecheck Map.empty (typeCheck (Apply (Lambda "x" TInt (Var "x")) ln)) === Right TInt

propArithOp :: HH.Property
propArithOp = HH.property $ do
    op <- HH.forAll genArithOp
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    runTypecheck Map.empty (typeCheck (Op op lm ln)) === Right TInt

propArithMismatchLeft :: HH.Property
propArithMismatchLeft = HH.property $ do
    op <- HH.forAll genArithOp
    b <- HH.forAll Gen.bool
    let lb = Lit . LBool $ b
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    runTypecheck Map.empty (typeCheck (Op op lb ln)) === Left (Mismatch TBool TInt)

propArithMismatchRight :: HH.Property
propArithMismatchRight = HH.property $ do
    op <- HH.forAll genArithOp
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    b <- HH.forAll Gen.bool
    let lb = Lit . LBool $ b
    runTypecheck Map.empty (typeCheck (Op op ln lb)) === Left (Mismatch TInt TBool)


-- unit testing

units :: TestTree
units = testGroup "Unit Tests" [letExample, fixExample, typeTests]

letExample :: TestTree
letExample = testCase "let x = 3 in x evaluates to 3" $ do
    let letExpression = Let "x" (Lit (LInt 3)) (Var "x")
    eval Map.empty letExpression @?= VInt 3

fixExample :: TestTree
fixExample = testCase "factorial 3 == 6" $ do
    let factorial = Fix (Lambda "fact" (TFunction TInt TInt) (Lambda "x" TInt (If (Op Equals (Var "x") (Lit (LInt 0))) (Lit (LInt 1)) (Op Multiply (Var "x") (Apply (Var "fact") (Op Subtract (Var "x") (Lit (LInt 1))))))))
    eval Map.empty (Apply factorial (Lit (LInt 3))) @?= VInt 6

typeTests :: TestTree
typeTests = testGroup "Unit tests of typechecker" [ funcAndVar
                                                  , outOfScope
                                                  , appliedToWrongType
                                                  , applyNonFunction
                                                  , nonBoolIfCondition
                                                  , mismatchedThenElse
                                                  , mismatchedEqualsLeft
                                                  , mismatchedEqualsRight
                                                  , correctIfElse
                                                  , correctEquals
                                                  , correctFix
                                                  , fixMismatch
                                                  , fixNonFunction
                                                  , correctApply
                                                  , correctLet
                                                  , letScopeCheck
                                                  , letInnerError
                                                  , letOuterError
                                                  ]

funcAndVar :: TestTree
funcAndVar = testCase "typeof (\\x : Int -> x) = TFunction TInt TInt" $ do
    let lambdaExpr = Lambda "x" TInt (Var "x")
    runTypecheck Map.empty (typeCheck lambdaExpr) @?= Right (TFunction TInt TInt)

outOfScope :: TestTree
outOfScope = testCase "typeof x == NotInScope" $ do
    runTypecheck Map.empty (typeCheck (Var "x")) @?= Left (NotInScope "x")

appliedToWrongType :: TestTree
appliedToWrongType = testCase "typeof (\\x : Int -> x) True == Mismatch TBool TInt" $ do
    let lambdaExpr = Lambda "x" TInt (Var "x")
    runTypecheck Map.empty (typeCheck (Apply lambdaExpr (Lit (LBool True)))) @?= Left (Mismatch TBool TInt)

applyNonFunction :: TestTree
applyNonFunction = testCase "typeof (1 1) == NotFunction TInt" $ do
    runTypecheck Map.empty (typeCheck (Apply (Lit (LInt 1)) (Lit (LInt 1)))) @?= Left (NotFunction TInt)

nonBoolIfCondition :: TestTree
nonBoolIfCondition = testCase "typeof (if 1 then True else False) == Mismatch TInt TBool" $ do
    let ifExpr = If (Lit (LInt 1)) (Lit (LBool True)) (Lit (LBool False))
    runTypecheck Map.empty (typeCheck ifExpr) @?= Left (Mismatch TInt TBool)

mismatchedThenElse :: TestTree
mismatchedThenElse = testCase "typeof (if True then 1 else False) == Mismatch TInt TBool" $ do
    let ifExpr = If (Lit (LBool True)) (Lit (LInt 1)) (Lit (LBool False))
    runTypecheck Map.empty (typeCheck ifExpr) @?= Left (Mismatch TInt TBool)

mismatchedEqualsLeft :: TestTree
mismatchedEqualsLeft = testCase "typeof (True == 1) == Mismatch TBool TInt" $ do
    let eqExpr = Op Equals (Lit (LBool True)) (Lit (LInt 1)) 
    runTypecheck Map.empty (typeCheck eqExpr) @?= Left (Mismatch TBool TInt)

mismatchedEqualsRight :: TestTree
mismatchedEqualsRight = testCase "typeof (1 == True) == Mismatch TInt TBool" $ do
    let eqExpr = Op Equals (Lit (LInt 1)) (Lit (LBool True))
    runTypecheck Map.empty (typeCheck eqExpr) @?= Left (Mismatch TInt TBool)

correctIfElse :: TestTree
correctIfElse = testCase "typeof (if True then 1 else 2) == TInt" $ do
    let ifExpr = If (Lit (LBool True)) (Lit (LInt 1)) (Lit (LInt 2))
    runTypecheck Map.empty (typeCheck ifExpr) @?= Right TInt

correctEquals :: TestTree
correctEquals = testCase "typeof (1 == 2) == TBool" $ do
    let eqExpr = Op Equals (Lit (LInt 1)) (Lit (LInt 2))
    runTypecheck Map.empty (typeCheck eqExpr) @?= Right TBool

correctFix :: TestTree
correctFix = testCase "typeof Fix factorial == TFunction TInt TInt" $ do
    let factorial = Fix (Lambda "fact" (TFunction TInt TInt) (Lambda "x" TInt (If (Op Equals (Var "x") (Lit (LInt 0))) (Lit (LInt 1)) (Op Multiply (Var "x") (Apply (Var "fact") (Op Subtract (Var "x") (Lit (LInt 1))))))))
    runTypecheck Map.empty (typeCheck factorial) @?= Right (TFunction TInt TInt)

fixMismatch :: TestTree
fixMismatch = testCase "typeof Fix (\\x : Bool -> 1) == Mismatch TBool TInt" $ do
    let badFunc = Fix (Lambda "x" TBool (Lit (LInt 1)))
    runTypecheck Map.empty (typeCheck badFunc) @?= Left (Mismatch TBool TInt)

fixNonFunction :: TestTree
fixNonFunction = testCase "typeof Fix 1 == NotFunction TInt" $ do
    runTypecheck Map.empty (typeCheck (Fix (Lit (LInt 1)))) @?= Left (NotFunction TInt)

correctApply :: TestTree
correctApply = testCase "typeof ((\\x : Int -> x)1) == TInt" $ do
    let applyExpr = Apply (Lambda "x" TInt (Var "x")) (Lit (LInt 1))
    runTypecheck Map.empty (typeCheck applyExpr) @?= Right TInt

correctLet :: TestTree
correctLet = testCase "typeof (let x = 1 in True) == TBool" $ do
    let letExpr = Let "x" (Lit (LInt 1)) (Lit (LBool True))
    runTypecheck Map.empty (typeCheck letExpr) @?= Right TBool

-- make sure that the "let x = a" part of let...in is in scope when checking "in b"
letScopeCheck :: TestTree
letScopeCheck = testCase "typeof (let x = 1 in x) == TInt" $ do
    let letExpr = Let "x" (Lit (LInt 1)) (Var "x")
    runTypecheck Map.empty (typeCheck letExpr) @?= Right TInt

-- error in the "let x = a" part of let...in
letInnerError :: TestTree
letInnerError = testCase "typeof (let x = ((\\n : Int -> 1)True) in True) == Mismatch TBool TInt" $ do
    let letExpr = Let "x" (Apply (Lambda "n" TInt (Lit (LInt 1))) (Lit (LBool True))) (Lit (LBool True))
    runTypecheck Map.empty (typeCheck letExpr) @?= Left (Mismatch TBool TInt)

-- error in the "in b" part of let...in
letOuterError :: TestTree
letOuterError = testCase "typeof (let x = 1 in y) == NotInScope" $ do
    let letExpr = Let "x" (Lit (LInt 1)) (Var "y")
    runTypecheck Map.empty (typeCheck letExpr) @?= Left (NotInScope "y")
