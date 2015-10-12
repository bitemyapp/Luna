{-# LANGUAGE OverloadedStrings #-}

module Main where

import Math.Symbolic.Expr
import Math.Symbolic.Rewrite

-- TODO: need way of determining which rewrite is closest match - then we can make a proper eval function!

main :: IO ()
main = return ()

testA :: MathExpression
testA = MApply "fibs" [10]

fj (Just x) = x

fibs0 = RewriteApply "fibs" [0] :=> 0
fibs1 = RewriteApply "fibs" [1] :=> 1
fibsn = RewriteApply "fibs" ["n"] :=> (RewriteApply "fibs" ["n" - 1] + RewriteApply "fibs" ["n" - 2])
fibs = [fibs0, fibs1, fibsn]
