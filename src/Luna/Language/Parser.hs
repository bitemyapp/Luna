{-# LANGUAGE OverloadedStrings #-}

module Luna.Language.Parser where

import Data.Text

import Control.Monad
import Control.Applicative hiding ((<|>))

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

import Luna.Language.Expr
import Luna.Language.Rewrite

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

lunaParens = parens lunaLang
lunaBrackets = brackets lunaLang
lunaComma = comma lunaLang
lunaReservedOp = reservedOp lunaLang
lunaIdentifier = identifier lunaLang
lunaInteger = integer lunaLang

{--------------------------------------------------------------------
    Expressions
--------------------------------------------------------------------}

expr :: Parser Expression
expr = try (lunaParens expr) <|> try exprApply <|> exprLiteral

exprApply :: Parser Expression
exprApply = EApply <$> lunaIdentifier <*> lunaBrackets (expr `sepBy` lunaComma)

exprLiteral :: Parser Expression
exprLiteral = liftM ELit (try (LInteger <$> lunaInteger) <|>
                          try (LBool <$> (True <$ string "True" <|> False <$ string "False")) <|>
                          try (LVar <$> lunaIdentifier <*> rewritePat))

rewritePat :: Parser (Maybe RewritePattern)
rewritePat = try satisfies <|> try anything <|> return Nothing
  where
      anything = char '_' >> return (Just Anything)
      satisfies = char '_' >> char '?' >> liftM Just (Satisfies <$> lunaIdentifier)
