module Luna.Language.Expr where

import Data.String
import Data.Ratio

{--------------------------------------------------------------------
    Identifiers
--------------------------------------------------------------------}

type Identifier = String

{--------------------------------------------------------------------
    Literals
--------------------------------------------------------------------}

data Literal
    = LInteger Integer
    | LBool Bool
    | LVar Identifier (Maybe RewritePattern)

instance Show Literal where
    show (LInteger x) = show x
    show (LBool x) = show x
    show (LVar x y) = maybe x (\pat -> x ++ show pat) y

instance Eq Literal where
    LInteger a == LInteger b = a == b
    LBool a == LBool b = a == b
    LVar a b == LVar c d = a == c && b == d
    _ == _ = False

{--------------------------------------------------------------------
    Rewrite patterns
--------------------------------------------------------------------}

data RewritePattern
    = Anything
    | Satisfies Identifier
    deriving (Eq)

instance Show RewritePattern where
    show Anything = "_"
    show (Satisfies p) = "_?" ++ p

exprIsMatch :: Expression -> RewritePattern -> Bool
exprIsMatch _ Anything = True
exprIsMatch e (Satisfies "IntegerQ") = isInteger e
    where
        isInteger (ELit (LInteger _)) = True
        isInteger _ = False

{--------------------------------------------------------------------
    Expressions
--------------------------------------------------------------------}

data Expression
    = ELit Literal
    | EApply Identifier [Expression]

apply0E :: Identifier -> Expression
apply0E f = EApply f []

apply1E :: Identifier -> Expression -> Expression
apply1E f x = EApply f [x]

apply2E :: Identifier -> Expression -> Expression -> Expression
apply2E f x y = EApply f [x, y]

instance Show Expression where
    show (ELit x) = show x
    show (EApply x y) = x ++ show y

instance Eq Expression where
    ELit a == ELit b = a == b
    (EApply f x) == (EApply g y) = f == g && x == y
    _ == _ = False

instance IsString Expression where
    fromString x = ELit (LVar x Nothing)

instance Num Expression where
    (+) = apply2E "Add"
    (-) = apply2E "Subtract"
    (*) = apply2E "Multiply"
    abs = apply1E "Abs"
    negate = apply1E "Negate"
    signum = apply1E "Sign"
    fromInteger = ELit . LInteger

instance Fractional Expression where
    (/) = apply2E "Divide"
    recip x = 1 / x
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Floating Expression where
    pi = apply0E "Pi"
    exp = apply1E "Exp"
    log = apply1E "Log"
    sqrt = apply1E "Sqrt"
    (**) = apply2E "Power"
    logBase = apply2E "LogBase"
    sin = apply1E "Sin"
    cos = apply1E "Cos"
    tan = apply1E "Tan"
    asin = apply1E "ArcSin"
    acos = apply1E "ArcCos"
    atan = apply1E "ArcTan"
    sinh = apply1E "Sinh"
    cosh = apply1E "Cosh"
    tanh = apply1E "Tanh"
    asinh = apply1E "ArcSinh"
    acosh = apply1E "ArcCosh"
    atanh = apply1E "ArcTanh"
