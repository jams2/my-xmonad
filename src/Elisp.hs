{-# LANGUAGE DeriveDataTypeable #-}

module Elisp
  ( elisp,
    prettyPrint,
    ElispExpr(..)
  )
where

import Data.Data
import Data.Functor ((<&>))
import Data.Void
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type ElispParser = Parsec Void String

data ElispExpr
  = EString String
  | ESymbol String
  | EQuoted ElispExpr
  | EList [ElispExpr]
  | EDot
  deriving (Show, Data)

sc :: ElispParser ()
sc = L.space space1 empty empty

lexeme :: ElispParser String -> ElispParser String
lexeme = L.lexeme sc

symbol :: String -> ElispParser String
symbol = L.symbol sc

parens :: ElispParser a -> ElispParser a
parens = between (Elisp.symbol "(") (Elisp.symbol ")")

symChar :: ElispParser Char
symChar =
  alphaNumChar
    <|> char '!'
    <|> char '#'
    <|> char '@'
    <|> char '$'
    <|> char '%'
    <|> char '^'
    <|> char '&'
    <|> char '*'
    <|> char '_'
    <|> char '-'
    <|> char '+'
    <|> char '='
    <|> char '/'
    <|> char '<'
    <|> char '>'

eStringLiteral :: ElispParser ElispExpr
eStringLiteral =
  Elisp.lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))
    <&> EString
    <?> "string literal"

eSymbol :: ElispParser ElispExpr
eSymbol = Elisp.lexeme (some symChar) <&> ESymbol <?> "symbol"

eAtom :: ElispParser ElispExpr
eAtom = eSymbol <|> eStringLiteral

eDot :: ElispParser ElispExpr
eDot = Elisp.symbol "." >> return EDot

eQuoted :: ElispParser ElispExpr
eQuoted = char '\'' *> (eAtom <|> eList) <&> EQuoted

eList :: ElispParser ElispExpr
eList = parens (many (eAtom <|> eQuoted <|> eDot <|> eList)) <&> EList

elispExpr :: ElispParser ElispExpr
elispExpr = eQuoted <|> eAtom <|> eList

elisp :: QuasiQuoter
elisp =
  QuasiQuoter
    { quoteExp = \s ->
        case runParser (sc *> elispExpr) "" s of
          Left err -> error $ errorBundlePretty err
          Right expr -> liftData expr,
      quotePat = \s -> error $ "Can't parse elisp in pattern position: " ++ s,
      quoteDec = \s -> error $ "Can't parse elisp in declaration position: " ++ s,
      quoteType = \s -> error $ "Can't parse elisp in type position: " ++ s
    }

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint ElispExpr where
  prettyPrint (EString s) = show s
  prettyPrint (ESymbol s) = s
  prettyPrint EDot = "."
  prettyPrint (EQuoted e) = "'" ++ prettyPrint e
  prettyPrint (EList es) = "(" ++ unwords (map prettyPrint es) ++ ")"
