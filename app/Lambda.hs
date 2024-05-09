{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- reimplementing https://www.cs.tufts.edu/comp/150FP/archive/geoff-mainland/quasiquoting.pdf
-- uses some from https://well-typed.com/blog/2014/10/quasi-quoting-dsls/

module Lambda where

import Control.Monad (ap)
import Data.Char (isLower)
import Data.Generics (Data, extQ)
import Data.Void (Void)
import Language.Haskell.TH (ExpQ, PatQ, mkName, runIO, varE, varP)
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ, dataToPatQ)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Parsing boilerplate
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> fail $ errorBundlePretty err
    Right a -> return a

topLevel :: Parser a -> Parser a
topLevel p = space *> p <* eof

-- untyped lambda calculus
data Var
  = V String
  | AV String
  deriving (Show, Eq, Data)

vs :: Var -> String
vs (V x) = x
vs (AV x) = x

data Exp
  = Var Var
  | Lam Var Exp
  | App Exp Exp
  | AE String
  deriving (Show, Data, Eq)

-- untyped lambda calculus parser, including antiquotation
ident :: Parser String
ident = lexeme $
  do
    c <- satisfy $ ap ((&&) . isLower) (/= 'λ')
    cs <- many (alphaNumChar <|> char '_' <|> char '\'')
    return (c : cs)

var :: Parser Var
var = (V <$> ident) <|> (AV <$> (string "$v:" >> ident))

pexp :: Parser Exp
pexp = foldl1 App <$> many aexp

{- ORMOLU_DISABLE -}
aexp :: Parser Exp
aexp = 
    (try $ Var <$> var)
    <|>
    do lexeme $ (char '\\' <|> char 'λ')
       v <- var
       lexeme $ char '.'
       Lam v <$> pexp
    <|>
    (between (lexeme $ char '(') (lexeme $ char ')') pexp)
    <|>
    (AE <$> (string "$e:" >> ident))
{- ORMOLU_ENABLE -}

-- setting up quasiquoting
antiVarE :: Var -> Maybe ExpQ
antiVarE (AV v) = Just $ varE $ mkName v
antiVarE _ = Nothing

antiExpE :: Exp -> Maybe ExpQ
antiExpE (AE v) = Just $ varE $ mkName v
antiExpE _ = Nothing

antiVarP :: Var -> Maybe PatQ
antiVarP (AV v) = Just $ varP $ mkName v
antiVarP _ = Nothing

antiExpP :: Exp -> Maybe PatQ
antiExpP (AE v) = Just $ varP $ mkName v
antiExpP _ = Nothing

ex :: QuasiQuoter
ex =
  QuasiQuoter
    { quoteExp = \str -> (runIO $ parseIO (topLevel pexp) str) >>= dataToExpQ (const Nothing `extQ` antiVarE `extQ` antiExpE),
      quotePat = \str -> (runIO $ parseIO (topLevel pexp) str) >>= dataToPatQ (const Nothing `extQ` antiVarP `extQ` antiExpP),
      quoteDec = undefined,
      quoteType = undefined
    }
