module Lambda.Calculus.Combinators
    ( lookupCombinator
    , expandCombinators
    , expandWith
    , knownCombinators
    , knownCombinatorsValues
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Lambda.Calculus.AST (Expr(..))

-- | Definitions of common named combinators expanded into core lambda calculus.
knownCombinators :: Map String Expr
knownCombinators = Map.fromList
    [ ("I", Lam "x" (Var "x"))
    , ("K", Lam "x" (Lam "y" (Var "x")))
    , ("S", Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))))
    , ("B", Lam "f" (Lam "g" (Lam "x" (App (Var "f") (App (Var "g") (Var "x"))))))
    , ("C", Lam "f" (Lam "x" (Lam "y" (App (App (Var "f") (Var "y")) (Var "x")))))
    , ("W", Lam "f" (Lam "x" (App (App (Var "f") (Var "x")) (Var "x"))))
    , ("Y", Lam "f" (App inner inner))
    ]
  where
    inner = Lam "x" (App (Var "f") (App (Var "x") (Var "x")))

lookupCombinator :: String -> Maybe Expr
lookupCombinator name = Map.lookup name knownCombinators

expandCombinators :: Expr -> Expr
expandCombinators = expandWith knownCombinators

-- | Expand free variables that are backed by a definition map.
-- Bound variables are left untouched and cycles fall back to the original name.
expandWith :: Map String Expr -> Expr -> Expr
expandWith env expr = go Set.empty Set.empty expr
  where
    go :: Set String -> Set String -> Expr -> Expr
    go bound visiting current = case current of
        Var v
            | v `Set.member` bound -> Var v
            | v `Set.member` visiting -> Var v
            | otherwise ->
                case Map.lookup v env of
                    Nothing -> Var v
                    Just replacement -> go bound (Set.insert v visiting) replacement
        Lam v body -> Lam v (go (Set.insert v bound) visiting body)
        App f x     -> App (go bound visiting f) (go bound visiting x)

knownCombinatorsValues :: [Expr]
knownCombinatorsValues = Map.elems knownCombinators
