module Luna.Interpreter.Environment where

import Data.Maybe

import Control.Monad.State

import Luna.Language.Definition
import Luna.Language.Parser

{--------------------------------------------------------------------
    Environment
--------------------------------------------------------------------}

type Luna = StateT Environment IO

runLuna :: Luna a -> IO a
runLuna luna = do
    -- Read in the prelude file
    prelude <- readFile "Prelude.luna"
    case parseLunaFile "Prelude.luna" prelude of
        Left e -> error $ show e
        Right prelude -> do
            -- Extract the rules
            let initRules = mapMaybe stmtRules prelude
            -- Run using the initial environment
            evalStateT luna (Environment initRules)

data Environment
    = Environment {
         envRules :: [(Expression, Expression)]
    }

initEnv :: Environment
initEnv = Environment {
        envRules = []
    }
