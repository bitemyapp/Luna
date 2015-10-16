module Luna.Interpreter.Environment where

import Control.Monad.State

import Luna.Language.Expr

{--------------------------------------------------------------------
    Environment
--------------------------------------------------------------------}

type Luna = StateT Environment IO

runLuna :: Luna a -> IO a
runLuna luna = evalStateT luna initEnv

data Environment
    = Environment {
         envRules :: [(Expression, Expression)]
    }

initEnv :: Environment
initEnv = Environment {
        envRules = [(apply1E "fibs" 0, 0),
                    (apply1E "fibs" 1, 1),
                    (apply1E "fibs" (EVar "n" (Just (Satisfies "IntegerQ"))), apply1E "fibs" (varE "n" - 1) + apply1E "fibs" (varE "n" - 2))]
    }
