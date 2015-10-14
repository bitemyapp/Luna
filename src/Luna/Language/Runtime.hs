{-# LANGUAGE OverloadedStrings #-}

module Luna.Language.Runtime where

import Data.Maybe
import qualified Data.Map as M

import Luna.Language.Expr
import Luna.Language.Rewrite

{--------------------------------------------------------------------
    Environment
--------------------------------------------------------------------}

data Environment
    = Environment {
         envRules :: [(Expression, Expression)]
    }

initialEnvironment :: Environment
initialEnvironment = Environment {
        envRules = fibs
    }

fibs = [(EApply "Fibs" [0], 0),
        (EApply "Fibs" [1], 1),
        (EApply "Fibs" [ELit (LVar "n" (Just (Satisfies "IntegerQ")))], EApply "Fibs" ["n" - 1] + EApply "Fibs" ["n" - 2])]

{--------------------------------------------------------------------
    Simplification
--------------------------------------------------------------------}

iteratively :: Eq a => (a -> a) -> a -> a
iteratively f x = untilEqual (iterate f x)
    where
        untilEqual (x:y:xs) | x == y = x
                            | otherwise = untilEqual xs

evaluate :: Expression -> Expression
evaluate = iteratively doEval
    where
        doEval expr =
            case expr of
                EApply "Hold" xs -> EApply "Hold" xs
                EApply x xs ->
                    maybe (EApply x (fmap evaluate xs))
                          (\f -> fromMaybe (EApply x (fmap evaluate xs)) (f xs))
                          (M.lookup x builtIns)
                other -> other
        builtIns = M.fromList [("Add", lunaAdd),
                               ("Subtract", lunaSubtract),
                               ("Multiply", lunaMultiply),
                               ("IntegerQ", lunaIntegerQ),
                               ("BooleanQ", lunaBooleanQ)]

{--------------------------------------------------------------------
    Built-ins
--------------------------------------------------------------------}

lunaAdd :: [Expression] -> Maybe Expression
lunaAdd [ELit (LInteger a), ELit (LInteger b)] = Just $ ELit (LInteger (a + b))
lunaAdd _ = Nothing

lunaSubtract :: [Expression] -> Maybe Expression
lunaSubtract [ELit (LInteger a), ELit (LInteger b)] = Just $ ELit (LInteger (a - b))
lunaSubtract _ = Nothing

lunaMultiply :: [Expression] -> Maybe Expression
lunaMultiply [ELit (LInteger a), ELit (LInteger b)] = Just $ ELit (LInteger (a * b))
lunaMultiply _ = Nothing

lunaIntegerQ :: [Expression] -> Maybe Expression
lunaIntegerQ [ELit (LInteger _)] = Just $ ELit (LBool True)
lunaIntegerQ [_] = Just $ ELit (LBool False)
lunaIntegerQ _ = Nothing

lunaBooleanQ :: [Expression] -> Maybe Expression
lunaBooleanQ [ELit (LBool _)] = Just $ ELit (LBool True)
lunaBooleanQ [_] = Just $ ELit (LBool False)
lunaBooleanQ _ = Nothing
