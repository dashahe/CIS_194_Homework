{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
    deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
            | Comb [SExpr]
    deriving Show

parseAtom :: Parser SExpr
parseAtom = A <$> ((N <$> posInt) <|> (I <$> ident))

parseCombs :: Parser SExpr
parseCombs = Comb <$> zeroOrMore (parseSExpr <* spaces)

parseSExpr :: Parser SExpr
parseSExpr = (leftParen *> parseCombs <* rightParen) <|> parseAtom 
                where leftParen  = (:) <$> char '(' <*> spaces
                      rightParen = (:) <$> char ')' <*> spaces

main :: IO()
main = do
    code <- readFile "testCode.scm"
    putStrLn (show (runParser parseSExpr code))
