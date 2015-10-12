module Math.Symbolic.Rewrite where

import Safe

import Data.Maybe
import Data.String
import Data.Ord
import Data.List
import Data.Traversable
import qualified Data.Map as M

import Control.Monad
import Control.Arrow
import Control.Applicative

import Math.Symbolic.Expr

data RewriteRule = RewriteExpression :=> RewriteExpression
    deriving (Show)
infixr 0 :=>

rulePattern :: RewriteRule -> RewriteExpression
rulePattern (pat :=> _) = pat

ruleTarget :: RewriteRule -> RewriteExpression
ruleTarget (_ :=> tar) = tar

ruleInverse :: RewriteRule -> RewriteRule
ruleInverse (a :=> b) = b :=> a

data RewriteExpression
    = RewriteVar Identifier
    | RewriteInteger Integer
    | RewriteApply Identifier [RewriteExpression]
    | RewriteExpression :~+~: RewriteExpression
    | RewriteExpression :~-~: RewriteExpression
    | RewriteExpression :~*~: RewriteExpression
    | RewriteExpression :~/~: RewriteExpression
    | RewriteExpression :~^~: RewriteExpression

instance Show RewriteExpression where
    show (RewriteVar x) = show x
    show (RewriteInteger x) = show x
    show (RewriteApply f x) = show f ++ "(" ++ show x ++ ")"
    show (x :~+~: y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (x :~-~: y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (x :~*~: y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (x :~/~: y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (x :~^~: y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"

instance IsString RewriteExpression where
    fromString = RewriteVar

instance Num RewriteExpression where
    (+) = (:~+~:)
    (-) = (:~-~:)
    (*) = (:~*~:)
    abs = error "RewriteExpression::Num::abs"   -- TODO
    negate = error "RewriteExpression::Num::negate"   -- TODO
    signum = error "RewriteExpression::Num::signum"   -- TODO
    fromInteger = RewriteInteger

instance Fractional RewriteExpression where
    (/) = (:~/~:)
    recip x = 1 / x
    fromRational = error "RewriteExpression::Fractional::fromRational"   -- TODO

instance Floating RewriteExpression where
    pi = error "RewriteExpression::Floating::pi"   -- TODO
    exp = error "RewriteExpression::Floating::exp"   -- TODO
    log = error "RewriteExpression::Floating::log"   -- TODO
    sqrt = error "RewriteExpression::Floating::sqrt"   -- TODO
    (**) = (:~^~:)
    logBase a b = log a / log b
    sin = error "RewriteExpression::Floating::sin"   -- TODO
    cos = error "RewriteExpression::Floating::cos"   -- TODO
    tan = error "RewriteExpression::Floating::tan"   -- TODO
    asin = error "RewriteExpression::Floating::asin"   -- TODO
    acos = error "RewriteExpression::Floating::acos"   -- TODO
    atan = error "RewriteExpression::Floating::atan"   -- TODO
    sinh = error "RewriteExpression::Floating::sinh"   -- TODO
    cosh = error "RewriteExpression::Floating::cosh"   -- TODO
    tanh = error "RewriteExpression::Floating::tanh"   -- TODO
    asinh = error "RewriteExpression::Floating::asinh"   -- TODO
    acosh = error "RewriteExpression::Floating::acosh"   -- TODO
    atanh = error "RewriteExpression::Floating::atanh"   -- TODO

eval :: [RewriteRule] -> MathExpression -> Maybe MathExpression
eval rules expr = maybe Nothing (\e -> maybe (Just e) (eval rules) (stepEval rules e)) (stepEval rules expr)

stepEval :: [RewriteRule] -> MathExpression -> Maybe MathExpression
stepEval rules expr =
    let ruleScore = (id &&& (rewriteScore . rulePattern)) <$> rules
        sortedRules = fst <$> sortBy (\(_, a) (_, b) -> compare (Down a) (Down b)) ruleScore
    in simplifyE <$> headMay (mapMaybe (`rewriteRecursive` expr) sortedRules)

rewriteScore :: RewriteExpression -> Integer
rewriteScore rule =
    case rule of
        RewriteVar _ -> 0
        RewriteInteger _ -> 1
        RewriteApply _ fx -> 1 + sum (fmap rewriteScore fx)
        a :~+~: b -> 1 + rewriteScore a + rewriteScore b
        a :~-~: b -> 1 + rewriteScore a + rewriteScore b
        a :~*~: b -> 1 + rewriteScore a + rewriteScore b
        a :~/~: b -> 1 + rewriteScore a + rewriteScore b
        a :~^~: b -> 1 + rewriteScore a + rewriteScore b

rewrite :: RewriteRule -> MathExpression -> Maybe MathExpression
rewrite (a :=> b) e = do
    rewrites <- rewritePattern a e
    applyRewrite b rewrites

rewriteRecursive :: RewriteRule -> MathExpression -> Maybe MathExpression
rewriteRecursive rule@(a :=> b) expr = mplus (rewrite rule expr) (rewriteSubparts expr)
    where
        rewriteSubparts e =
            case e of
                {-
                | MApply Identifier MathExpression
                | MIntegral Identifier MathExpression
                | (:-) MathExpression
                -}
                a :+: b -> rewriteBinOp (+) a b
                a :-: b -> rewriteBinOp (-) a b
                a :*: b -> rewriteBinOp (*) a b
                a :/: b -> rewriteBinOp (/) a b
                a :^: b -> rewriteBinOp (^) a b
                other -> rewrite rule other
        rewriteBinOp f a b =
            case (rewriteRecursive rule a, rewriteRecursive rule b) of
                (Just a', Just b') -> return (a' + b')
                (Just a', Nothing) -> return (a' + b)
                (Nothing, Just b') -> return (a + b')
                (Nothing, Nothing) -> Nothing

rewritePattern :: RewriteExpression -> MathExpression -> Maybe (M.Map Identifier MathExpression)
rewritePattern rewrite input = rewriteMatcher rewrite input M.empty
    where
        rewriteMatcher rExpr mExpr curr =
            case (rExpr, mExpr) of
                (RewriteVar a, x) -> addIfNotContains a x curr
                (RewriteInteger x, MInteger y) ->
                    if x == y
                        then return curr
                        else Nothing
                (RewriteApply f xs, MApply g ys) ->
                    if f /= g
                        then Nothing
                        else undefined $ combineMaybes (zipWith rewritePattern xs ys)                           -- TODO
                (a :~+~: b, x :+: y) -> rewriteBinOp (a, x) (b, y) curr
                (a :~-~: b, x :-: y) -> rewriteBinOp (a, x) (b, y) curr
                (a :~*~: b, x :*: y) -> rewriteBinOp (a, x) (b, y) curr
                (a :~/~: b, x :/: y) -> rewriteBinOp (a, x) (b, y) curr
                (a :~^~: b, x :^: y) -> rewriteBinOp (a, x) (b, y) curr
                _ -> Nothing
        combineMaybes :: [Maybe a] -> Maybe [a]
        combineMaybes [] = Just []
        combineMaybes (Nothing:_) = Nothing
        combineMaybes (Just x:xs) = (x:) <$> combineMaybes xs
        addIfNotContains :: Identifier -> MathExpression -> M.Map Identifier MathExpression -> Maybe (M.Map Identifier MathExpression)
        addIfNotContains a x vals =
            maybe (return (M.insert a x vals))
                (\aVal -> if aVal == x then return vals else Nothing)
                (M.lookup a vals)
        rewriteBinOp :: (RewriteExpression, MathExpression) -> (RewriteExpression, MathExpression) -> M.Map Identifier MathExpression -> Maybe (M.Map Identifier MathExpression)
        rewriteBinOp (a, x) (b, y) vals = do
            aMatches <- rewriteMatcher a x vals
            rewriteMatcher b y aMatches

applyRewrite :: RewriteExpression -> M.Map Identifier MathExpression -> Maybe MathExpression
applyRewrite rRule rVals =
    case rRule of
        RewriteVar k -> M.lookup k rVals
        RewriteInteger x -> return (MInteger x)
        RewriteApply f xs -> do
            body <- sequenceA (fmap (`applyRewrite` rVals) xs)
            return (MApply f body)
        a :~+~: b -> rewriteBinOp a b (+)
        a :~-~: b -> rewriteBinOp a b (-)
        a :~*~: b -> rewriteBinOp a b (*)
        a :~/~: b -> rewriteBinOp a b (/)
        a :~^~: b -> rewriteBinOp a b (**)
    where
        rewriteBinOp :: RewriteExpression -> RewriteExpression -> (MathExpression -> MathExpression -> MathExpression) -> Maybe MathExpression
        rewriteBinOp a b op = do
            x <- applyRewrite a rVals
            y <- applyRewrite b rVals
            return (x `op` y)
