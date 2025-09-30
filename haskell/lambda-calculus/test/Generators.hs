module Generators
    ( genExpr
    , genVar
    ) where

import Lambda.Calculus (Expr(..))
import qualified Lambda.Calculus.Combinators as Comb
import Test.QuickCheck

varPool :: [String]
varPool = ["x", "y", "z", "a", "b", "c", "I", "K", "S", "B", "C", "W", "Y"]

genVar :: Gen String
genVar = elements varPool

genExpr :: Int -> Gen Expr
genExpr size
    | size <= 0 = leaf
    | otherwise = frequency
        [ (4, Var <$> genVar)
        , (2, Lam <$> genVar <*> genExpr (size - 1))
        , (2, do
                leftSize <- choose (0, size - 1)
                let rightSize = size - 1 - leftSize
                App <$> genExpr leftSize <*> genExpr rightSize)
        , (1, elements combinatorSamples)
        ]
  where
    leaf = frequency
        [ (5, Var <$> genVar)
        , (1, elements combinatorSamples)
        ]

combinatorSamples :: [Expr]
combinatorSamples = Comb.knownCombinatorsValues
