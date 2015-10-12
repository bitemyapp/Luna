module Math.Symbolic.Expr where

import Data.String

type Identifier = String

data MathConstant
    = CE
    | CPi
    deriving (Eq)

instance Show MathConstant where
    show CE = "e"
    show CPi = "pi"

data MathExpression
    = MConst MathConstant
    | MInteger Integer
    | MRational Rational
    | MVar Identifier
    | MApply Identifier [MathExpression]
    | (:-) MathExpression
    | MathExpression :+: MathExpression
    | MathExpression :-: MathExpression
    | MathExpression :*: MathExpression
    | MathExpression :/: MathExpression
    | MathExpression :^: MathExpression

apply1 :: Identifier -> MathExpression -> MathExpression
apply1 f x = MApply f [x]

instance Show MathExpression where
    show (MConst x) = show x
    show (MInteger x) = show x
    show (MVar x) = x
    show (MApply x y) = x ++ "(" ++ show y ++ ")"
    show ((:-) x) = "-(" ++ show x ++ ")"
    show (x :+: y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (x :-: y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (x :*: y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (x :/: y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (x :^: y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

instance Eq MathExpression where
    MConst a == MConst b = a == b
    MInteger a == MInteger b = a == b
    MRational a == MRational b = a == b
    MVar a == MVar b = a == b
    (MApply f x) == (MApply g y) = f == g && x == y
    ((:-) a) == ((:-) b) = a == b
    (a :+: b) == (c :+: d) = a == c && b == d
    (a :-: b) == (c :-: d) = a == c && b == d
    (a :*: b) == (c :*: d) = a == c && b == d
    (a :/: b) == (c :/: d) = a == c && b == d
    (a :^: b) == (c :^: d) = a == c && b == d
    _ == _ = False

instance IsString MathExpression where
    fromString = MVar

instance Num MathExpression where
    (+) = (:+:)
    (-) = (:-:)
    (*) = (:*:)
    abs = apply1 "abs"
    negate = (:-)
    signum = apply1 "sgn"
    fromInteger = MInteger

instance Fractional MathExpression where
    (/) = (:/:)
    recip x = 1 / x
    fromRational = MRational

instance Floating MathExpression where
    pi = MConst CPi
    exp = apply1 "exp"
    log = apply1 "log"
    sqrt = apply1 "sqrt"
    (**) = (:^:)
    logBase a b = log a / log b
    sin = apply1 "sin"
    cos = apply1 "cos"
    tan = apply1 "tan"
    asin = apply1 "asin"
    acos = apply1 "acos"
    atan = apply1 "atan"
    sinh = apply1 "sinh"
    cosh = apply1 "cosh"
    tanh = apply1 "tanh"
    asinh = apply1 "asinh"
    acosh = apply1 "acosh"
    atanh = apply1 "atanh"

iteratively :: Eq a => (a -> a) -> a -> a
iteratively f x = untilEqual (iterate f x)
    where
        untilEqual (x:y:xs) | x == y = x
                            | otherwise = untilEqual xs

simplifyE :: MathExpression -> MathExpression
simplifyE = iteratively doEval
    where
        doEval expr =
            case expr of
                -- Apply constants!
                MInteger a :+: MInteger b -> MInteger (a + b)
                MInteger a :-: MInteger b -> MInteger (a - b)
                MInteger a :*: MInteger b -> MInteger (a * b)
                -- Iterative
                MApply x fx -> MApply x (fmap simplifyE fx)
                (:-) a -> (:-) (simplifyE a)
                a :+: b -> simplifyE a + simplifyE b
                a :-: b -> simplifyE a - simplifyE b
                a :*: b -> simplifyE a * simplifyE b
                a :/: b -> simplifyE a / simplifyE b
                a :^: b -> simplifyE a ** simplifyE b
                other -> other
