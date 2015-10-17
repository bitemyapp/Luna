module Luna.Interpreter.Interpreter where

import System.IO

import Control.Monad.State

import Luna.Interpreter.Rewrite
import Luna.Interpreter.Environment
import Luna.Language.Definition
import Luna.Language.Parser

{--------------------------------------------------------------------
    Interpreter
--------------------------------------------------------------------}

lunaREPL :: Luna ()
lunaREPL = do
    -- Get the input
    lunaPrint "In[??]  = "
    input <- lunaRead
    lunaPrint "\n"
    -- Parse the input
    case parseLuna input of
        Left e ->
            -- Print the error
            lunaPrint (show e)
        Right stmt ->
            case stmt of
                SExpr expr -> do
                    -- Print the result
                    evaledExpr <- eval expr
                    lunaPrint ("Out[??] = " ++ show evaledExpr)
                SRule pat result -> do
                    state (\s -> ((), Environment { envRules = (pat, result) : envRules s }))
                    lunaPrint ("Out[??] = " ++ show stmt)
    lunaPrint "\n\n"
    -- And repeat
    lunaREPL

lunaPrint :: String -> Luna ()
lunaPrint s = liftIO (putStr s >> hFlush stdout)

lunaRead :: Luna String
lunaRead = liftIO getLine
