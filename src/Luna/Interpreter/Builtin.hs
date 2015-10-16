module Luna.Interpreter.Builtin where

import Data.Maybe
import qualified Data.Map as M

import Luna.Language.Definition

{--------------------------------------------------------------------
    Simplification
--------------------------------------------------------------------}

evaluate :: Expression -> Expression
evaluate e = untilEqual (iterate doEval e)
    where
        untilEqual (x:y:xs) | x == y = x
                            | otherwise = untilEqual xs
        doEval expr =
            case expr of
                EApply (EVar x m) xs ->
                    maybe (EApply (EVar x m) (fmap evaluate xs))
                          (\f -> fromMaybe (EApply (EVar x m) (fmap evaluate xs)) (f xs))
                          (M.lookup x builtIns)
                EApply x xs ->
                    EApply x (fmap evaluate xs)
                other -> other
        builtIns = M.fromList [("Add", lunaAdd),
            ("Subtract", lunaSubtract),
            ("Multiply", lunaMultiply),
            ("Negate", lunaNegate),
            ("IntegerQ", lunaIntegerQ),
            ("SameQ", lunaSameQ)]

{--------------------------------------------------------------------
    Built-ins
--------------------------------------------------------------------}

lunaAdd :: [Expression] -> Maybe Expression
lunaAdd [EInteger a, EInteger b] = return (EInteger (a + b))
lunaAdd _ = Nothing

lunaSubtract :: [Expression] -> Maybe Expression
lunaSubtract [EInteger a, EInteger b] = return (EInteger (a - b))
lunaSubtract _ = Nothing

lunaMultiply :: [Expression] -> Maybe Expression
lunaMultiply [EInteger a, EInteger b] = return (EInteger (a * b))
lunaMultiply _ = Nothing

lunaNegate :: [Expression] -> Maybe Expression
lunaNegate [EInteger a] = return (EInteger (-a))
lunaNegate _ = Nothing

lunaIntegerQ :: [Expression] -> Maybe Expression
lunaIntegerQ [EInteger _] = return (varE "True")
lunaIntegerQ [_] = return (varE "False")
lunaIntegerQ _ = Nothing

lunaSameQ :: [Expression] -> Maybe Expression
lunaSameQ [x, y] = return (varE (show (x == y)))
lunaSameQ _ = Nothing
