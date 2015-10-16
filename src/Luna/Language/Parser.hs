module Luna.Language.Parser where

import Control.Monad
import Control.Applicative hiding ((<|>), many)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

import Luna.Language.Expr

{--------------------------------------------------------------------
    Parser
--------------------------------------------------------------------}

parseLuna :: String -> Either ParseError Expression
parseLuna = runParser (expr >>= (\e -> eof >> return e)) () "<<interpreted>>"

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
        opLetter	    = oneOf ":!#$%&*+./<=>?@\\^|-~",
        reservedOpNames = ["->", "^", "*", "/", "+", "-"],
        reservedNames   = [],
        caseSensitive   = True
    }

{--------------------------------------------------------------------
    Expressions
--------------------------------------------------------------------}

expr = parens lunaLang expr <|> exprList <|> exprInteger <|> exprChar <|> exprString <|> try exprApply <|> exprVar

exprList = EList <$> braces lunaLang (commaSep lunaLang expr)
exprApply = EApply <$> exprVar <*> brackets lunaLang (commaSep lunaLang expr)     -- TODO: allow expressions like Fibs[][]
exprInteger = EInteger <$> integer lunaLang
exprChar = EChar <$> charLiteral lunaLang
exprString = (EList . fmap EChar) <$> stringLiteral lunaLang
exprVar = EVar <$> identifier lunaLang <*> rewritePat

rewritePat = optionMaybe (try satisfiesPat <|> try anythingPat)
satisfiesPat = string "_?" >> (Satisfies <$> identifier lunaLang)
anythingPat = char '_' >> return Anything

{--------------------------------------------------------------------
    Statements
--------------------------------------------------------------------}
