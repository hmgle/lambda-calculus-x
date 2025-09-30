module Lambda.Calculus.AST
    ( Expr(..)
    , prettyExpr
    , freeVars
    , freshName
    , renameBound
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Lambda calculus expressions with named binders.
data Expr
    = Var String
    | Lam String Expr
    | App Expr Expr
    deriving (Eq, Ord)

instance Show Expr where
    show = prettyExpr

data Ctx = TopCtx | AppLeftCtx | AppRightCtx deriving (Eq)

-- | Render an expression using standard lambda calculus notation.
prettyExpr :: Expr -> String
prettyExpr = render TopCtx
  where
    render :: Ctx -> Expr -> String
    render ctx expr = case expr of
        Var name -> name
        Lam{}    -> wrapIf (ctx /= TopCtx) $ "\\" ++ unwords params ++ ". " ++ render TopCtx body
          where
            (params, body) = collectLam expr
        App f x  -> wrapIf (ctx == AppRightCtx) $ render AppLeftCtx f ++ " " ++ render AppRightCtx x

    wrapIf :: Bool -> String -> String
    wrapIf True  s = "(" ++ s ++ ")"
    wrapIf False s = s

    collectLam :: Expr -> ([String], Expr)
    collectLam (Lam v b) = let (vs, inner) = collectLam b in (v : vs, inner)
    collectLam other     = ([], other)

-- | Compute the free variables of an expression.
freeVars :: Expr -> Set String
freeVars expr = case expr of
    Var v     -> Set.singleton v
    Lam v b   -> Set.delete v (freeVars b)
    App f arg -> freeVars f `Set.union` freeVars arg

-- | Produce a fresh variable name not present in the provided set.
freshName :: String -> Set String -> String
freshName base taken = head $ dropWhile (`Set.member` taken) candidates
  where
    candidates = [base ++ replicate n '\'' | n <- [0 ..]]

-- | Rename bound occurrences of a variable, avoiding shadowed binders.
renameBound :: String -> String -> Expr -> Expr
renameBound target replacement = go
  where
    go expr = case expr of
        Var v
            | v == target -> Var replacement
            | otherwise   -> Var v
        Lam v body
            | v == target -> Lam v body
            | otherwise   -> Lam v (go body)
        App f x -> App (go f) (go x)
