module Main where

import Luna.Interpreter.Builtin
import Luna.Interpreter.Environment
import Luna.Interpreter.Interpreter
import Luna.Interpreter.Rewrite
import Luna.Language.Definition
import Luna.Language.Parser

main :: IO ()
main = runLuna lunaREPL

-- Rule

-- Replace           = rewrite
-- ReplaceALl        = rewriteRecursive
-- ReplaceRepeated   = eval
-- ReplaceList       = stepEval (modified)
