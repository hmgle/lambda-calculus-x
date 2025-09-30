module Lib
    ( module Lambda.Calculus
    , evaluateAndFormat
    , evaluateAndFormatWithLimit
    , evaluateAndFormatWithStrategy
    , evaluateAndFormatWith
    , formatResult
    ) where

import Lambda.Calculus

-- | Parse, normalize, and pretty-print a lambda calculus expression using the default reducer.
evaluateAndFormat :: String -> Either String String
evaluateAndFormat = evaluateAndFormatWith NormalOrder defaultLimit

-- | Same as 'evaluateAndFormat' but with an explicit step limit.
evaluateAndFormatWithLimit :: Int -> String -> Either String String
evaluateAndFormatWithLimit limit = evaluateAndFormatWith NormalOrder limit

-- | Evaluate with a specific reduction strategy.
evaluateAndFormatWithStrategy :: Strategy -> String -> Either String String
evaluateAndFormatWithStrategy strategy = evaluateAndFormatWith strategy defaultLimit

-- | Core evaluation helper used by the CLI to pick both strategy and limit.
evaluateAndFormatWith :: Strategy -> Int -> String -> Either String String
evaluateAndFormatWith strategy limit input =
    case evaluateTextWithStrategyLimit strategy limit input of
        Left err     -> Left (renderCalculusError err)
        Right result -> Right (formatResult result)

-- | Format the final expression with the number of beta-reduction steps used.
formatResult :: EvalResult -> String
formatResult result =
    let rendered = prettyExpr (resultExpr result)
        suffix = "  -- " ++ show (stepsUsed result) ++ " step" ++ plural (stepsUsed result)
    in rendered ++ suffix
  where
    plural 1 = ""
    plural _ = "s"
