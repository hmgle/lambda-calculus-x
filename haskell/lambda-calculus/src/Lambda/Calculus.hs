module Lambda.Calculus
    ( Expr(..)
    , prettyExpr
    , parseExpr
    , ParserError(..)
    , renderParserError
    , Strategy(..)
    , EvalError(..)
    , EvalResult(..)
    , normalize
    , normalizeWithLimit
    , normalizeWithStrategy
    , normalizeWithStrategyLimit
    , CalculusError(..)
    , renderCalculusError
    , evaluateText
    , evaluateTextWithLimit
    , evaluateTextWithStrategy
    , evaluateTextWithStrategyLimit
    , defaultLimit
    ) where

import Lambda.Calculus.AST (Expr(..), prettyExpr)
import Lambda.Calculus.Eval
    ( EvalError(..)
    , EvalResult(..)
    , Strategy(..)
    , normalize
    , normalizeWithLimit
    , normalizeWithStrategy
    , normalizeWithStrategyLimit
    )
import qualified Lambda.Calculus.Eval as Eval
import Lambda.Calculus.Parser
    ( ParserError(..)
    , parseExpr
    , renderParserError
    )

data CalculusError
    = ParseFailure ParserError
    | EvalFailure EvalError

instance Show CalculusError where
    show = renderCalculusError

renderCalculusError :: CalculusError -> String
renderCalculusError err = case err of
    ParseFailure perr -> renderParserError perr
    EvalFailure eerr  -> Eval.renderEvalError eerr

evaluateText :: String -> Either CalculusError EvalResult
evaluateText = evaluateTextWithStrategyLimit NormalOrder defaultLimit

evaluateTextWithLimit :: Int -> String -> Either CalculusError EvalResult
evaluateTextWithLimit limit = evaluateTextWithStrategyLimit NormalOrder limit

evaluateTextWithStrategy :: Strategy -> String -> Either CalculusError EvalResult
evaluateTextWithStrategy strategy = evaluateTextWithStrategyLimit strategy defaultLimit

evaluateTextWithStrategyLimit :: Strategy -> Int -> String -> Either CalculusError EvalResult
evaluateTextWithStrategyLimit strategy limit input = do
    expr <- either (Left . ParseFailure) Right (parseExpr input)
    either (Left . EvalFailure) Right (normalizeWithStrategyLimit strategy limit expr)

defaultLimit :: Int
defaultLimit = 1000
