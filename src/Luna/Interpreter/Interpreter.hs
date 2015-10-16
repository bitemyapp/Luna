module Luna.Interpreter.Interpreter where

import Control.Monad.State

import Luna.Interpreter.Rewrite
import Luna.Interpreter.Environment
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
        Right expr -> do
            -- Print the result
            evaledExpr <- eval expr
            lunaPrint ("Out[??] = " ++ show evaledExpr)
    lunaPrint "\n\n"
    -- And repeat
    lunaREPL

lunaPrint :: String -> Luna ()
lunaPrint = liftIO . putStr

lunaRead :: Luna String
lunaRead = liftIO getLine
