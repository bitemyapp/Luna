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
        builtIns = M.fromList (fmap lunaBuiltin [lunaAdd, lunaSubtract, lunaMultiply, lunaNegate, lunaIntegerQ])

{--------------------------------------------------------------------
    Built-ins
--------------------------------------------------------------------}

data LunaBuiltin
    = LunaBuiltin Identifier ([Expression] -> Maybe Expression)

lunaBuiltin :: LunaBuiltin -> (Identifier, [Expression] -> Maybe Expression)
lunaBuiltin (LunaBuiltin f fx) = (f, fx)

lunaAdd :: LunaBuiltin
lunaAdd = LunaBuiltin "Add" f
    where
        f [EInteger a, EInteger b] = return (EInteger (a + b))
        f _ = Nothing

lunaSubtract :: LunaBuiltin
lunaSubtract = LunaBuiltin "Subtract" f
    where
        f [EInteger a, EInteger b] = return (EInteger (a - b))
        f _ = Nothing

lunaMultiply :: LunaBuiltin
lunaMultiply = LunaBuiltin "Multiply" f
    where
        f [EInteger a, EInteger b] = return (EInteger (a * b))
        f _ = Nothing

lunaNegate :: LunaBuiltin
lunaNegate = LunaBuiltin "Negate" f
    where
        f [EInteger a] = return (EInteger (-a))
        f _ = Nothing

lunaIntegerQ :: LunaBuiltin
lunaIntegerQ = LunaBuiltin "IntegerQ" f
    where
        f [EInteger _] = return (varE "True")
        f [_] = return (varE "False")
        f _ = Nothing
