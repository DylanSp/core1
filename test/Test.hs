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
combinators = testGroup "Lambda calculus combinators" [identity, kConst, skki]

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
intType = testProperty "typeof (any integer) == tInt" propIntType
boolType = testProperty "typeof (any boolean) == tBool" propBoolType
appliedFuncType = testProperty "typeof (\\x : Int -> x) (any integer) == tInt" propAppliedFunc
arithOpType = testProperty "typeof (any Add/Subtract/Multiply) (any int) (any int) == tInt" propArithOp
mismatchedArithOpLeft = testProperty "typeof (Add/Subtract/Multiply) (any bool) (any int) == Mismatch tBool tInt" propArithMismatchLeft
mismatchedArithOpRight = testProperty "typeof (Add/Subtract/Multiply) (any int) (any bool) == Mismatch tInt tBool" propArithMismatchRight

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
    typeOf ln === Right tInt

propBoolType :: HH.Property
propBoolType = HH.property $ do
    b <- HH.forAll Gen.bool
    let lb = Lit . LBool $ b
    typeOf lb === Right tBool

propAppliedFunc :: HH.Property
propAppliedFunc = HH.property $ do
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    typeOf (Apply (Lambda "x" tInt (Var "x")) ln) === Right tInt

propArithOp :: HH.Property
propArithOp = HH.property $ do
    op <- HH.forAll genArithOp
    m <- HH.forAll genAnyInt
    let lm = Lit . LInt $ m
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    typeOf (Op op lm ln) === Right tInt

propArithMismatchLeft :: HH.Property
propArithMismatchLeft = HH.property $ do
    op <- HH.forAll genArithOp
    b <- HH.forAll Gen.bool
    let lb = Lit . LBool $ b
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    typeOf (Op op lb ln) === Left (Mismatch tBool tInt)

propArithMismatchRight :: HH.Property
propArithMismatchRight = HH.property $ do
    op <- HH.forAll genArithOp
    n <- HH.forAll genAnyInt
    let ln = Lit . LInt $ n
    b <- HH.forAll Gen.bool
    let lb = Lit . LBool $ b
    typeOf (Op op ln lb) === Left (Mismatch tInt tBool)


-- unit testing

units :: TestTree
units = testGroup "Unit Tests" [letExample, fixExample, typeTests, programExample]

letExample :: TestTree
letExample = testCase "let x = 3 in x evaluates to 3" $ do
    let letExpression = Let "x" (Lit (LInt 3)) (Var "x")
    eval Map.empty letExpression @?= VInt 3

fixExample :: TestTree
fixExample = testCase "factorial 3 == 6" $ do
    let factorial = Fix (Lambda "fact" (TFunction tInt tInt) (Lambda "x" tInt (If (Op Equals (Var "x") (Lit (LInt 0))) (Lit (LInt 1)) (Op Multiply (Var "x") (Apply (Var "fact") (Op Subtract (Var "x") (Lit (LInt 1))))))))
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
                                                  , polymorphicLambda
                                                  , polymorphicLambdaMulti
                                                  , polymorphicApplied
                                                  , polymorphicAppliedMulti
                                                  , polymorphicPartialApplied
                                                  ]

funcAndVar :: TestTree
funcAndVar = testCase "typeof (\\x : Int -> x) = TFunction tInt tInt" $ do
    let lambdaExpr = Lambda "x" tInt (Var "x")
    typeOf lambdaExpr @?= Right (TFunction tInt tInt)

outOfScope :: TestTree
outOfScope = testCase "typeof x == NotInScope" $ do
    typeOf (Var "x") @?= Left (NotInScope "x")

appliedToWrongType :: TestTree
appliedToWrongType = testCase "typeof (\\x : Int -> x) True == Mismatch tBool tInt" $ do
    let lambdaExpr = Lambda "x" tInt (Var "x")
    typeOf (Apply lambdaExpr (Lit (LBool True))) @?= Left (Mismatch tBool tInt)

applyNonFunction :: TestTree
applyNonFunction = testCase "typeof (1 1) == NotFunction tInt" $ do
    typeOf (Apply (Lit (LInt 1)) (Lit (LInt 1))) @?= Left (NotFunction tInt)

nonBoolIfCondition :: TestTree
nonBoolIfCondition = testCase "typeof (if 1 then True else False) == Mismatch tInt tBool" $ do
    let ifExpr = If (Lit (LInt 1)) (Lit (LBool True)) (Lit (LBool False))
    typeOf ifExpr @?= Left (Mismatch tInt tBool)

mismatchedThenElse :: TestTree
mismatchedThenElse = testCase "typeof (if True then 1 else False) == Mismatch tInt tBool" $ do
    let ifExpr = If (Lit (LBool True)) (Lit (LInt 1)) (Lit (LBool False))
    typeOf ifExpr @?= Left (Mismatch tInt tBool)

mismatchedEqualsLeft :: TestTree
mismatchedEqualsLeft = testCase "typeof (True == 1) == Mismatch tBool tInt" $ do
    let eqExpr = Op Equals (Lit (LBool True)) (Lit (LInt 1)) 
    typeOf eqExpr @?= Left (Mismatch tBool tInt)

mismatchedEqualsRight :: TestTree
mismatchedEqualsRight = testCase "typeof (1 == True) == Mismatch tInt tBool" $ do
    let eqExpr = Op Equals (Lit (LInt 1)) (Lit (LBool True))
    typeOf eqExpr @?= Left (Mismatch tInt tBool)

correctIfElse :: TestTree
correctIfElse = testCase "typeof (if True then 1 else 2) == tInt" $ do
    let ifExpr = If (Lit (LBool True)) (Lit (LInt 1)) (Lit (LInt 2))
    typeOf ifExpr @?= Right tInt

correctEquals :: TestTree
correctEquals = testCase "typeof (1 == 2) == tBool" $ do
    let eqExpr = Op Equals (Lit (LInt 1)) (Lit (LInt 2))
    typeOf eqExpr @?= Right tBool

correctFix :: TestTree
correctFix = testCase "typeof Fix factorial == TFunction tInt tInt" $ do
    let factorial = Fix (Lambda "fact" (TFunction tInt tInt) (Lambda "x" tInt (If (Op Equals (Var "x") (Lit (LInt 0))) (Lit (LInt 1)) (Op Multiply (Var "x") (Apply (Var "fact") (Op Subtract (Var "x") (Lit (LInt 1))))))))
    typeOf factorial @?= Right (TFunction tInt tInt)

fixMismatch :: TestTree
fixMismatch = testCase "typeof Fix (\\x : Bool -> 1) == Mismatch tBool tInt" $ do
    let badFunc = Fix (Lambda "x" tBool (Lit (LInt 1)))
    typeOf badFunc @?= Left (Mismatch tBool tInt)

fixNonFunction :: TestTree
fixNonFunction = testCase "typeof Fix 1 == NotFunction tInt" $ do
    typeOf (Fix (Lit (LInt 1))) @?= Left (NotFunction tInt)

correctApply :: TestTree
correctApply = testCase "typeof ((\\x : Int -> x)1) == tInt" $ do
    let applyExpr = Apply (Lambda "x" tInt (Var "x")) (Lit (LInt 1))
    typeOf applyExpr @?= Right tInt

correctLet :: TestTree
correctLet = testCase "typeof (let x = 1 in True) == tBool" $ do
    let letExpr = Let "x" (Lit (LInt 1)) (Lit (LBool True))
    typeOf letExpr @?= Right tBool

-- make sure that the "let x = a" part of let...in is in scope when checking "in b"
letScopeCheck :: TestTree
letScopeCheck = testCase "typeof (let x = 1 in x) == tInt" $ do
    let letExpr = Let "x" (Lit (LInt 1)) (Var "x")
    typeOf letExpr @?= Right tInt

-- error in the "let x = a" part of let...in
letInnerError :: TestTree
letInnerError = testCase "typeof (let x = ((\\n : Int -> 1)True) in True) == Mismatch tBool tInt" $ do
    let letExpr = Let "x" (Apply (Lambda "n" tInt (Lit (LInt 1))) (Lit (LBool True))) (Lit (LBool True))
    typeOf letExpr @?= Left (Mismatch tBool tInt)

-- error in the "in b" part of let...in
letOuterError :: TestTree
letOuterError = testCase "typeof (let x = 1 in y) == NotInScope" $ do
    let letExpr = Let "x" (Lit (LInt 1)) (Var "y")
    typeOf letExpr @?= Left (NotInScope "y")

-- convert to property test?
polymorphicLambda :: TestTree
polymorphicLambda = testCase "typeof (\\x : a -> x) == TFunction (TVariable\"a\") (TVariable \"a\")" $ do
    let idExpr = Lambda "x" (TVariable "a") (Var "x")
    typeOf idExpr @?= Right (TFunction (TVariable "a") (TVariable "a"))

-- convert to property test?
polymorphicLambdaMulti :: TestTree
polymorphicLambdaMulti = testCase "typeof (\\x : a -> (\\y : b -> x)) == TFunction (TVariable \"a\" (TFunction (TVariable \"b\") (TVariable \"a\"))" $ do
    let constExpr = Lambda "x" (TVariable "a") (Lambda "y" (TVariable "b") (Var "x"))
    typeOf constExpr @?= Right (TFunction (TVariable "a") (TFunction (TVariable "b") (TVariable "a")))

-- convert to property test?
polymorphicApplied :: TestTree
polymorphicApplied = testCase "typeof ((\\x : a -> x)True) == tBool" $ do
    let idExpr = Lambda "x" (TVariable "a") (Var "x")
    typeOf (Apply idExpr (Lit (LBool True))) @?= Right tBool

-- convert to property test?
polymorphicAppliedMulti :: TestTree
polymorphicAppliedMulti = testCase "typeof (((\\x : a -> (\\y : b -> x))3)True) == tInt" $ do
    let constExpr = Lambda "x" (TVariable "a") (Lambda "y" (TVariable "b") (Var "x"))
    typeOf (Apply (Apply constExpr (Lit (LInt 3))) (Lit (LBool True))) @?= Right tInt

-- convert to property test?
polymorphicPartialApplied :: TestTree
polymorphicPartialApplied = testCase "typeof ((\\x : a -> (\\y : b -> x))3) == TFunction (TVariable\"b\") tInt" $ do
    let constExpr = Lambda "x" (TVariable "a") (Lambda "y" (TVariable "b") (Var "x"))
    typeOf (Apply constExpr (Lit (LInt 3))) @?= Right (TFunction (TVariable "b") tInt)

programExample :: TestTree
programExample = testCase "evalProgram (program to calculate 1 + 2) == 3" $ do
    let mDecl = ("m", Lit (LInt 1))
    let nDecl = ("n", Lit (LInt 2))
    let mainExpr = Op Add (Var "m") (Var "n")
    evalProgram (Program [mDecl, nDecl] mainExpr) @?= VInt 3