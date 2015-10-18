module Luna.Language.Parser where

import Control.Monad
import Control.Applicative hiding ((<|>), many)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

import Luna.Language.Definition

{--------------------------------------------------------------------
    Parser
--------------------------------------------------------------------}

parseLuna :: String -> Either ParseError Statement
parseLuna = runParser (stmt >>= (\s -> eof >> return s)) () "<<interpreted>>"

parseLunaFile :: FilePath -> String -> Either ParseError [Statement]
parseLunaFile = runParser (try (whiteSpace lunaLang) >> (stmt `sepEndBy` many newline) >>= (\stmts -> eof >> return stmts)) ()

{--------------------------------------------------------------------
    Language definition
--------------------------------------------------------------------}

lunaLang = makeTokenParser lunaLangDef
lunaLangDef = emptyDef {
        commentStart    = "{-",
        commentEnd      = "-}",
        commentLine     = "--",
        nestedComments  = True,
        identStart      = letter,
        identLetter	    = alphaNum,
        opStart	        = opLetter lunaLangDef,
        opLetter	    = oneOf "-+^*/:=_?",
        reservedOpNames = ["-", "+", "^", "*", "/", "<", ">", "<=", ">=", "==", "/=", "===", "=/=",
                           "/\\", "\\/",
                           ":=", "_?", "_"],
        reservedNames   = [],
        caseSensitive   = True
    }

{--------------------------------------------------------------------
    Expressions
--------------------------------------------------------------------}

expr = buildExpressionParser exprTable exprTerm
    where
        exprTable = [[prefix "-" negate, prefix "+" id],
                     [binary "^" (**) AssocRight],
                     [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
                     [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft],
                     [binary "<" (apply2E "LessThan") AssocLeft, binary "<=" (apply2E "LessEqualThan") AssocLeft,
                      binary ">" (apply2E "GreaterThan") AssocLeft, binary ">=" (apply2E "GreaterEqualThan") AssocLeft],
                     [binary "==" (apply2E "Equal") AssocLeft, binary "/=" (apply2E "NotEqual") AssocLeft,
                      binary "===" (apply2E "SameQ") AssocLeft, binary "=/=" (apply2E "NotSameQ") AssocLeft],
                     [binary "/\\" (apply2E "And") AssocLeft],
                     [binary "\\/" (apply2E "Or") AssocLeft]]
        binary name fun = Infix (reservedOp lunaLang name >> return fun)
        prefix name fun = Prefix (reservedOp lunaLang name >> return fun)
        postfix name fun = Postfix (reservedOp lunaLang name >> return fun)

exprTerm = parens lunaLang expr <|> exprList <|> exprInteger <|> exprChar <|> exprString <|> try exprApply <|> exprVar

exprList = EList <$> braces lunaLang (commaSep lunaLang expr)
exprApply = EApply <$> exprVar <*> brackets lunaLang (commaSep lunaLang expr)     -- TODO: allow expressions like Fibs[][]
exprInteger = EInteger <$> integer lunaLang
exprChar = EChar <$> charLiteral lunaLang
exprString = (EList . fmap EChar) <$> stringLiteral lunaLang
exprVar = EVar <$> identifier lunaLang <*> rewritePat

rewritePat = optionMaybe (anythingPat <|> satisfiesPat)
anythingPat = reservedOp lunaLang "_" >> return Anything
satisfiesPat = reservedOp lunaLang "_?" >> (Satisfies <$> identifier lunaLang)

{--------------------------------------------------------------------
    Statements
--------------------------------------------------------------------}

stmt = try stmtRule <|> stmtExpr

stmtExpr = SExpr <$> expr
stmtRule = do
    pat <- expr
    reservedOp lunaLang ":="
    result <- expr
    return (SRule pat result)
