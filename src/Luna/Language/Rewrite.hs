module Luna.Language.Rewrite where

import Data.Maybe
import Data.String
import Data.Ratio
import qualified Data.Map as M

import Control.Monad

import Luna.Language.Expr

{--------------------------------------------------------------------
    Expression rewriting
--------------------------------------------------------------------}

rewrite :: Expression -> Expression -> Expression -> Maybe Expression
rewrite a b e = do
    rewrites <- rewritePattern a e
    return (applyRewrite b rewrites)

rewriteRecursive :: Expression -> Expression -> Expression -> Maybe Expression
rewriteRecursive a b expr = mplus (rewrite a b expr) (rewriteSubparts expr)
    where
        rewriteSubparts e =
            case e of
                EApply f xs ->
                    let rewrittenArguments = fmap (rewriteRecursive a b) xs
                        rewritenWithFallback = zipWith fromMaybe xs rewrittenArguments
                    in if all isNothing rewrittenArguments
                        then Nothing
                        else return (EApply f rewritenWithFallback)
                other -> rewrite a b other

rewritePattern :: Expression -> Expression -> Maybe (M.Map Identifier Expression)
rewritePattern pat input = rewriteMatcher pat input M.empty
    where
        rewriteMatcher rPat mExpr curr =
            case (rPat, mExpr) of
                (ELit (LVar a m), x) ->
                    maybe (if ELit (LVar a m) == x then return curr else Nothing)
                          (\mPat -> if exprIsMatch x mPat
                              then maybe (return (M.insert a x curr))
                                         (\aVal -> if aVal == x then return curr else Nothing)
                                         (M.lookup a curr)
                              else Nothing)
                          m
                (ELit x, ELit y) ->
                    if x == y
                        then return curr
                        else Nothing
                (EApply f xs, EApply g ys) ->
                    if f == g
                        then rewriteLoop (zip xs ys) curr
                        else Nothing
                _ -> Nothing
        rewriteLoop :: [(Expression, Expression)] -> M.Map Identifier Expression -> Maybe (M.Map Identifier Expression)
        rewriteLoop [] curr = return curr
        rewriteLoop ((rExpr, mExpr):xs) curr = maybe Nothing (rewriteLoop xs) (rewriteMatcher rExpr mExpr curr)

applyRewrite :: Expression -> M.Map Identifier Expression -> Expression
applyRewrite rResult rVals =
    case rResult of
        ELit (LVar k m) -> fromMaybe (ELit (LVar k m)) (M.lookup k rVals)
        ELit x -> ELit x
        EApply f xs -> EApply f (fmap (`applyRewrite` rVals) xs)
