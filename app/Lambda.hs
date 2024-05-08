{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- reimplementing https://www.cs.tufts.edu/comp/150FP/archive/geoff-mainland/quasiquoting.pdf
-- uses some from https://well-typed.com/blog/2014/10/quasi-quoting-dsls/

module Lambda where

import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.String
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)
import Prelude hiding (exp)

import Control.Exception (throwIO)
import Data.Generics
import Data.Set (Set)
import qualified Data.Set          as Set

-- Parsing boilerplate
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

ident :: Parser String
ident = lexeme $ do c <- lowerChar; cs <- many (alphaNumChar <|> char '_'); return (c : cs)

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> throwIO (userError (show err))
    Right a  -> return a

topLevel :: Parser a -> Parser a
topLevel p = space *> p <* eof

-- untyped lambda calculus
data Var
  = V String
  deriving (Show, Eq, Ord, Data)

data LExp
  = Var Var
  | Lam Var LExp
  | App LExp LExp
  deriving (Show, Data)

-- untyped lambda calculus parser
pexp :: Parser LExp
pexp = foldl1 App <$> many aexp

{- ORMOLU_DISABLE -}
aexp :: Parser LExp
aexp = 
    try $ Var <$> V <$> ident
    <|>
    do lexeme $ (char '\\' <|> char 'l')
       v <- ident
       lexeme $ char '.'
       Lam (V v) <$> pexp
    <|>
    between (lexeme $ char '(') (lexeme $ char ')') pexp
{- ORMOLU_ENABLE -}

-- setting up quasiquoting

expr :: QuasiQuoter
expr = QuasiQuoter {
      quoteExp  = \str -> do
        -- TODO fix this for errors
        l <- location
        e <- runIO $ parseIO (topLevel pexp) str
        dataToExpQ (const Nothing `extQ` metaExp (free e)) e
      , quotePat = undefined
--    , quotePat  = \str -> do
--        -- TODO fix this for errors
--        l <- location
--        e <- runIO $ parseIO (topLevel pexp) str
--        dataToPatQ (const Nothing `extQ` metaPat (free e)) e
    , quoteDec  = undefined
    , quoteType = undefined
    }

metaExp :: Set Var -> LExp -> Maybe ExpQ
metaExp fvs (Var var@(V x)) | var `Set.member` fvs = Just [| toExpr $(varE (mkName x)) |]
metaExp _ _ = Nothing

metaPat :: Set Var -> LExp -> Maybe PatQ
metaPat fvs (Var var@(V x)) | var `Set.member` fvs = Just (varP (mkName x))
metaPat _ _ = Nothing

free :: LExp -> Set Var
free (Var v) = Set.singleton v
free (Lam v e) = Set.difference (free e) (Set.singleton v)
free (App e1 e2) = free e1 `Set.union` free e2

-- all applications have explicit parens
-- one layer of special case for single variables
pp :: LExp -> String
pp (Var (V x)) = x
pp (Lam (V x) e) = printf "λ%s. %s" x (pp e)
pp (App (Var (V x1)) (Var (V x2))) = printf "%s %s" x1 x2
pp (App (Var (V x)) e2) = printf "%s (%s)" x (pp e2)
pp (App e1 (Var (V x))) = printf "(%s) %s" (pp e1) x
pp (App e1 e2) = printf "(%s)(%s)" (pp e1) (pp e2)


