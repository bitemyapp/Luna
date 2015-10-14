module Main where

import Luna.Language.Expr
import Luna.Language.Parser
import Luna.Language.Rewrite
import Luna.Language.Interpreter

main :: IO ()
main = lunaREPL

-- Rule

-- Replace           = rewrite
-- ReplaceALl        = rewriteRecursive
-- ReplaceRepeated   = eval
-- ReplaceList       = stepEval (modified)
