module Luna.Language.Definition where

import Data.String
import Data.Ratio

{--------------------------------------------------------------------
    Identifiers
--------------------------------------------------------------------}

type Identifier = String

{--------------------------------------------------------------------
    Expressions
--------------------------------------------------------------------}

data Expression
    = EInteger Integer
    | EChar Char
    | EList [Expression]
    | EVar Identifier (Maybe RewritePattern)
    | EApply Expression [Expression]

instance Show Expression where
    show (EInteger x) = show x
    show (EChar x) = show x
    show (EList xs) = "{" ++ (init . tail . show) xs ++ "}"
    show (EVar x y) = maybe x (\pat -> x ++ show pat) y
    show (EApply x y) = show x ++ show y

instance Eq Expression where
    EInteger a == EInteger b = a == b
    EChar a == EChar b       = a == b
    EList as == EList bs     = and (zipWith (==) as bs)
    EVar a b == EVar c d     = a == c && b == d
    EApply f x == EApply g y = f == g && x == y
    _ == _ = False

instance IsString Expression where
    fromString str = EList (fmap EChar str)

instance Num Expression where
    (+) = apply2E "Add"
    (-) = apply2E "Subtract"
    (*) = apply2E "Multiply"
    abs = apply1E "Abs"
    negate = apply1E "Negate"
    signum = apply1E "Sign"
    fromInteger = EInteger

instance Fractional Expression where
    (/) = apply2E "Divide"
    recip x = 1 / x
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Floating Expression where
    pi = EVar "Pi" Nothing
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

varE :: Identifier -> Expression
varE x = EVar x Nothing

apply0E :: Identifier -> Expression
apply0E f = EApply (varE f) []

apply1E :: Identifier -> Expression -> Expression
apply1E f x = EApply (varE f) [x]

apply2E :: Identifier -> Expression -> Expression -> Expression
apply2E f x y = EApply (varE f) [x, y]

{--------------------------------------------------------------------
    Rewrite patterns
--------------------------------------------------------------------}

data RewritePattern
    = Anything
    | Satisfies Identifier

instance Show RewritePattern where
    show Anything = "_"
    show (Satisfies p) = "_?" ++ p

instance Eq RewritePattern where
    Anything == Anything       = True
    Satisfies x == Satisfies y = x == y
    _ == _ = False

{--------------------------------------------------------------------
    Statements
--------------------------------------------------------------------}

data Statement
    = SExpr Expression
    | SRule Expression Expression

instance Show Statement where
    show (SExpr x) = show x
    show (SRule x y) = show x ++ " := " ++ show y

instance Eq Statement where
    SExpr a == SExpr b = a == b
    SRule f x == SRule g y = f == g && x == y
    _ == _ = False

stmtRules :: Statement -> Maybe (Expression, Expression)
stmtRules (SRule pat result) = Just (pat, result)
stmtRules _ = Nothing
