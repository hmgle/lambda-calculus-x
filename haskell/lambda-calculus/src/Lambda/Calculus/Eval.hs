module Lambda.Calculus.Eval
    ( Strategy(..)
    , EvalError(..)
    , EvalResult(..)
    , normalize
    , normalizeWithLimit
    , normalizeWithStrategy
    , normalizeWithStrategyLimit
    , step
    , stepWithStrategy
    , renderEvalError
    ) where

import qualified Data.Set as Set
import Lambda.Calculus.AST
    ( Expr(..)
    , freshName
    , freeVars
    , prettyExpr
    , renameBound
    )

data Strategy
    = NormalOrder
    | ApplicativeOrder
    deriving (Eq, Show, Read, Enum, Bounded)

data EvalError = MaxStepsExceeded Int Expr deriving (Eq)

data EvalResult = EvalResult
    { stepsUsed :: Int
    , resultExpr :: Expr
    } deriving (Eq, Show)

instance Show EvalError where
    show = renderEvalError

renderEvalError :: EvalError -> String
renderEvalError (MaxStepsExceeded n expr) =
    "Evaluation did not converge within " ++ show n ++ " steps. Last expression: " ++ prettyExpr expr

normalize :: Expr -> Either EvalError EvalResult
normalize = normalizeWithStrategy NormalOrder

normalizeWithLimit :: Int -> Expr -> Either EvalError EvalResult
normalizeWithLimit = normalizeWithStrategyLimit NormalOrder

normalizeWithStrategy :: Strategy -> Expr -> Either EvalError EvalResult
normalizeWithStrategy strategy = normalizeWithStrategyLimit strategy defaultLimit
  where
    defaultLimit = 1000

normalizeWithStrategyLimit :: Strategy -> Int -> Expr -> Either EvalError EvalResult
normalizeWithStrategyLimit strategy limit expr
    | limit <= 0 = Left (MaxStepsExceeded 0 expr)
    | otherwise  = go 0 expr
  where
    go steps current
        | steps >= limit = Left (MaxStepsExceeded steps current)
        | otherwise =
            case stepWithStrategy strategy current of
                Nothing   -> Right (EvalResult steps current)
                Just next -> go (steps + 1) next

step :: Expr -> Maybe Expr
step = stepWithStrategy NormalOrder

stepWithStrategy :: Strategy -> Expr -> Maybe Expr
stepWithStrategy strategy expr = case strategy of
    NormalOrder      -> stepNormal expr
    ApplicativeOrder -> stepApplicative expr

stepNormal :: Expr -> Maybe Expr
stepNormal expr = case expr of
    App (Lam v body) arg -> Just (subst v arg body)
    App f arg ->
        case stepNormal f of
            Just f' -> Just (App f' arg)
            Nothing -> App f <$> stepNormal arg
    Lam v body -> Lam v <$> stepNormal body
    Var _      -> Nothing

stepApplicative :: Expr -> Maybe Expr
stepApplicative expr = case expr of
    App (Lam v body) arg
        | isValue arg -> Just (subst v arg body)
        | otherwise   -> App (Lam v body) <$> stepApplicative arg
    App f arg ->
        case stepApplicative f of
            Just f' -> Just (App f' arg)
            Nothing -> App f <$> stepApplicative arg
    Lam v body -> Lam v <$> stepApplicative body
    Var _      -> Nothing

isValue :: Expr -> Bool
isValue Lam{} = True
isValue _     = False

subst :: String -> Expr -> Expr -> Expr
subst name replacement expr = case expr of
    Var v
        | v == name -> replacement
        | otherwise -> Var v
    Lam v body
        | v == name -> Lam v body
        | v `Set.member` freeReplacement ->
            let taken = Set.unions [freeVars body, freeReplacement, Set.singleton name]
                v' = freshName v taken
                renamedBody = renameBound v v' body
            in Lam v' (subst name replacement renamedBody)
        | otherwise -> Lam v (subst name replacement body)
    App f arg -> App (subst name replacement f) (subst name replacement arg)
  where
    freeReplacement = freeVars replacement
