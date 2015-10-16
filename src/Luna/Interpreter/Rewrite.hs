module Luna.Interpreter.Rewrite where

import Safe

import Data.Maybe
import Data.String
import Data.List
import qualified Data.Map as M

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Arrow

import Luna.Interpreter.Builtin
import Luna.Interpreter.Environment
import Luna.Language.Expr

{--------------------------------------------------------------------
    Evaluation
--------------------------------------------------------------------}

eval :: Expression -> Luna Expression
eval e = do
    firstEval <- evalStep e
    fullEval firstEval e
    where
        fullEval curr old | curr == old = return curr
                          | otherwise   = do
                              new <- evalStep curr
                              fullEval new curr

evalStep :: Expression -> Luna Expression
evalStep expr = do
    -- Get the environment
    rules <- envRules <$> get
    -- Sort the rules
    let ruleScore = (id &&& (rewriteScore . fst)) <$> rules
        sortedRules = fst <$> sortBy (\(_, a) (_, b) -> compare b a) ruleScore
    -- Run the rules
    appliedRules <- mapM (\(pat, res) -> rewriteRecursive pat res expr) sortedRules
    -- Return either the initial input, or a rewritten input
    let result = evaluate (fromMaybe expr (headMay (catMaybes appliedRules)))
    return result

rewriteScore :: Expression -> Integer
rewriteScore rule =
    case rule of
        EInteger _ -> 10
        EChar _ -> 10
        EList xs -> 1 + sum (fmap rewriteScore xs)
        EVar _ (Just Anything) -> 0
        EVar _ (Just (Satisfies _)) -> 1
        EApply _ fx -> 1 + sum (fmap rewriteScore fx)

{--------------------------------------------------------------------
    Rewriting
--------------------------------------------------------------------}

satisfiesPattern :: Expression -> RewritePattern -> Luna Bool
satisfiesPattern e Anything = return True
satisfiesPattern e (Satisfies f) = do
    evalResult <- eval (EApply (EVar f Nothing) [e])
    return (varE "True" == evalResult)

rewrite :: Expression -> Expression -> Expression -> Luna (Maybe Expression)
rewrite a b e = do
    mayRewrites <- rewritePattern a e
    case mayRewrites of
        Just pat -> return (Just (applyRewrite b pat))
        Nothing -> return Nothing
    where
        rewritePattern :: Expression -> Expression -> Luna (Maybe (M.Map Identifier Expression))
        rewritePattern pat input = rewriteMatcher pat input M.empty
            where
                rewriteMatcher :: Expression -> Expression -> M.Map Identifier Expression -> Luna (Maybe (M.Map Identifier Expression))
                rewriteMatcher rPat mExpr curr =
                    case (rPat, mExpr) of
                        (EVar a m, b) ->
                            case m of
                                Nothing -> rewriteIfEqual (EVar a m) b curr
                                Just mPat -> do
                                    patternOk <- b `satisfiesPattern` mPat
                                    if patternOk
                                        then case M.lookup a curr of
                                            Just aVal -> rewriteIfEqual aVal b curr
                                            Nothing -> return (Just (M.insert a b curr))
                                        else return Nothing
                        (EApply f xs, EApply g ys) ->
                            if f == g
                                then rewriteLoop (zip xs ys) curr
                                else return Nothing
                        (x, y) -> rewriteIfEqual x y curr
                rewriteLoop :: [(Expression, Expression)] -> M.Map Identifier Expression -> Luna (Maybe (M.Map Identifier Expression))
                rewriteLoop [] curr = return (Just curr)
                rewriteLoop ((rExpr, mExpr):xs) curr = do
                    rewriteMatch <- rewriteMatcher rExpr mExpr curr
                    case rewriteMatch of
                        Just match -> rewriteLoop xs match
                        Nothing -> return Nothing
                rewriteIfEqual :: Expression -> Expression -> M.Map Identifier Expression -> Luna (Maybe (M.Map Identifier Expression))
                rewriteIfEqual a b curr
                    | a == b = return (Just curr)
                    | otherwise = return Nothing
        applyRewrite :: Expression -> M.Map Identifier Expression -> Expression
        applyRewrite rResult rVals =
            case rResult of
                EInteger x -> EInteger x
                EChar x -> EChar x
                EList xs -> EList (fmap (`applyRewrite` rVals) xs)
                EVar k m -> fromMaybe (EVar k m) (M.lookup k rVals)
                EApply f xs -> EApply f (fmap (`applyRewrite` rVals) xs)

rewriteRecursive :: Expression -> Expression -> Expression -> Luna (Maybe Expression)
rewriteRecursive a b expr = do
    fullRewrite <- rewrite a b expr
    subpartRewrite <- rewriteSubparts expr
    return (fullRewrite `mplus` subpartRewrite)
    where
        rewriteSubparts :: Expression -> Luna (Maybe Expression)
        rewriteSubparts e =
            case e of
                EApply f xs -> do
                    rewrittenArguments <- mapM (rewriteRecursive a b) xs
                    let rewritenWithFallback = zipWith fromMaybe xs rewrittenArguments
                    if all isNothing rewrittenArguments
                        then return Nothing
                        else return (Just (EApply f rewritenWithFallback))
                other -> rewrite a b other
