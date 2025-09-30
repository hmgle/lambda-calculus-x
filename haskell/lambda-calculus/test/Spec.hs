import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Property
    , counterexample
    , forAll
    , property
    , resize
    , sized
    , (===)
    , (.&&.)
    )

import Generators (genExpr)
import Lambda.Calculus
import qualified Lambda.Calculus.AST as AST
import qualified Lambda.Calculus.Combinators as Comb
import qualified Lambda.Calculus.Eval as Eval
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
    describe "parseExpr" $ do
        it "parses nested lambdas" $ do
            case parseExpr "\\x y. x" of
                Left err  -> expectationFailure ("Parse failed: " ++ renderParserError err)
                Right ast -> ast `shouldBe` Lam "x" (Lam "y" (Var "x"))

        it "parses left-associative application" $ do
            case parseExpr "a b c" of
                Left err  -> expectationFailure ("Parse failed: " ++ renderParserError err)
                Right ast -> ast `shouldBe` App (App (Var "a") (Var "b")) (Var "c")

    describe "normalize" $ do
        it "reduces the identity application" $ do
            case evaluateText "(\\x. x) y" of
                Left err   -> expectationFailure ("Unexpected error: " ++ show err)
                Right res -> do
                    stepsUsed res `shouldBe` 1
                    resultExpr res `shouldBe` Var "y"

        it "avoids variable capture" $ do
            case evaluateText "(\\x. \\y. x) y" of
                Left err -> expectationFailure ("Unexpected error: " ++ show err)
                Right res ->
                    case resultExpr res of
                        Lam binder body -> do
                            binder `shouldNotBe` "y"
                            body `shouldBe` Var "y"
                        other -> expectationFailure ("Expected lambda, got " ++ show other)

        it "signals divergence when exceeding the step limit" $ do
            case evaluateTextWithLimit 6 omega of
                Left (EvalFailure (MaxStepsExceeded steps _)) -> steps `shouldSatisfy` (>= 6)
                other -> expectationFailure ("Expected divergence, got " ++ show other)

    describe "strategies" $ do
        it "normal order ignores diverging arguments" $ do
            case evaluateTextWithStrategy NormalOrder ignoresOmega of
                Left err -> expectationFailure ("Unexpected error: " ++ show err)
                Right res -> resultExpr res `shouldBe` Var "y"

        it "applicative order evaluates diverging arguments" $ do
            case evaluateTextWithStrategyLimit ApplicativeOrder 8 ignoresOmega of
                Left (EvalFailure (MaxStepsExceeded steps _)) -> steps `shouldSatisfy` (>= 8)
                other -> expectationFailure ("Expected divergence, got " ++ show other)

    describe "combinators" $ do
        it "expands I to identity" $ do
            case parseExpr "I" of
                Left err -> expectationFailure ("Parse failed: " ++ renderParserError err)
                Right ast -> ast `shouldBe` Lam "x" (Var "x")

        it "does not rewrite bound I" $ do
            case parseExpr "\\I. I" of
                Left err -> expectationFailure ("Parse failed: " ++ renderParserError err)
                Right ast -> ast `shouldBe` Lam "I" (Var "I")

        it "evaluates SKK as identity" $ do
            case evaluateText "S K K z" of
                Left err -> expectationFailure ("Unexpected error: " ++ show err)
                Right res -> resultExpr res `shouldBe` Var "z"

        it "produces a fixed point with Y" $ do
            case evaluateText "Y (\\g. \\x. x)" of
                Left err -> expectationFailure ("Unexpected error: " ++ show err)
                Right res -> resultExpr res `shouldBe` Lam "x" (Var "x")

    describe "properties" $ do
        let maxSteps = 150
            sizedExpr = resize 8 (sized genExpr)

        modifyMaxSuccess (const 75) $ do
            prop "normalization yields a normal form" $
                forAll sizedExpr $ \expr ->
                    case normalizeWithStrategyLimit NormalOrder maxSteps expr of
                        Left _ -> property True
                        Right res ->
                            counterexample ("Result: " ++ show (resultExpr res)) $
                                Eval.stepWithStrategy NormalOrder (resultExpr res) === Nothing

            prop "normalization is idempotent on normal forms" $
                forAll sizedExpr $ \expr ->
                    case normalizeWithStrategyLimit NormalOrder maxSteps expr of
                        Left _ -> property True
                        Right res ->
                            case normalizeWithStrategyLimit NormalOrder maxSteps (resultExpr res) of
                                Left err -> counterexample ("Second normalization failed: " ++ show err) False
                                Right res2 ->
                                    counterexample ("Second result: " ++ show (resultExpr res2)) $
                                        (resultExpr res2 === resultExpr res)
                                        .&&. (stepsUsed res2 === 0)

            prop "combinator expansion removes free combinator names" $
                forAll sizedExpr $ \expr ->
                    let expanded = Comb.expandCombinators expr
                        freeNames = AST.freeVars expanded
                        offenders = Set.filter (
                            \name -> Comb.lookupCombinator name /= Nothing) freeNames
                    in counterexample ("Unexpected free combinators: " ++ show (Set.toList offenders))
                        (Set.null offenders)
  where
    omega = "(\\x. x x) (\\x. x x)"
    ignoresOmega = "(\\x. y) (" ++ omega ++ ")"
