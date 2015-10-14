module Luna.Language.Interpreter where

import Safe

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Arrow

import Luna.Language.Expr
import Luna.Language.Rewrite
import Luna.Language.Parser
import Luna.Language.Runtime

{--------------------------------------------------------------------
    Interpreter
--------------------------------------------------------------------}

lunaREPL :: IO ()
lunaREPL = evalStateT loop initialEnvironment
    where
        loop :: StateT Environment IO ()
        loop = do
            -- Get the input
            lunaPrint "In[??]  = "
            input <- lunaRead
            lunaPrint "\n"
            -- Parse the input
            case parseLuna input of
                Left e ->
                    -- Print the error
                    lunaPrint (show e)
                Right expr -> do
                    -- Print the result
                    evaledExpr <- eval expr
                    lunaPrint ("Out[??] = " ++ show evaledExpr)
            lunaPrint "\n\n"
            -- And repeat
            loop

lunaPrint :: String -> StateT Environment IO ()
lunaPrint = liftIO . putStr

lunaRead :: StateT Environment IO String
lunaRead = liftIO getLine

{--------------------------------------------------------------------
    Expression evaluation
--------------------------------------------------------------------}

eval :: Expression -> StateT Environment IO Expression
eval e = fullEval e 0
    where
        fullEval curr old | curr == old = return curr
                          | otherwise   = do
                              new <- evalStep curr
                              fullEval new curr

evalStep :: Expression -> StateT Environment IO Expression
evalStep expr = do
    -- Get the environment
    rules <- envRules <$> get
    -- Sort the rules
    let ruleScore = (id &&& (rewriteScore . fst)) <$> rules
        sortedRules = fst <$> sortBy (\(_, a) (_, b) -> compare b a) ruleScore
        appliedRules = mapMaybe (\(pat, res) -> rewriteRecursive pat res expr) sortedRules
    -- Return either the initial input, or a rewritten input
    let result = evaluate (fromMaybe expr (headMay appliedRules))
    return result

rewriteScore :: Expression -> Integer
rewriteScore rule =
    case rule of
        ELit (LVar _ (Just Anything)) -> 0
        ELit (LVar _ (Just (Satisfies _))) -> 1
        ELit _ -> 10
        EApply _ fx -> 1 + sum (fmap rewriteScore fx)
