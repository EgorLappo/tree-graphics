{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse where

import Control.Applicative ( Alternative((<|>), empty, some) )
import Data.Char ( isAlphaNum )
import Types ( Tree(..) )

placeholderLabel :: String
placeholderLabel = "xx"

nameInternalNodes :: Int -> String -> String
nameInternalNodes _ [] = []
nameInternalNodes i [s] | s == ')' = ")" ++ placeholderLabel ++ show i
                     | otherwise = [s]
nameInternalNodes i (x:y:xs) | x == ')' = if (y == ',') || (y == ')')
                                          then x:(placeholderLabel ++ show i ++ nameInternalNodes (i+1) (y:xs))
                                          else x : nameInternalNodes i (y:xs)
                             | otherwise = x : nameInternalNodes i (y:xs)

-- parse the Newick string into a Tree
parseString :: String -> Tree String
parseString = fst . head . parse tree . nameInternalNodes 1

-- the parser code is straight from the Hutton textbook
-- i don't need more complicated stuff for now
-- possible future update is to automatically detect unlabeled nodes

-- essentially, it's MaybeT of State, but using lists 
-- is simpler than unwrapping maybes
newtype Parser a = P (String -> [(a, String)]) 

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- ** type class definitions **
instance Functor Parser where
    fmap g (P p) = P (\inp -> case p inp of [] -> []
                                            [(v, r)] -> [(g v, r)])

instance Applicative Parser where
    pure x = P (\inp -> [(x, inp)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> p = P (\inp ->
        case parse pg inp of [] -> []
                             [(pf, r)] -> parse (fmap pf p) r)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp ->
        case parse p inp of [] -> []
                            [(v, r)] -> parse (f v) r)

instance Alternative Parser where
    empty = P (const [])
    p <|> q = P (\inp ->
        case parse p inp of [] -> parse q inp
                            [(v, r)] -> [(v, r)])

item :: Parser Char
item = P (\case
            [] -> []
            (x : xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat f = do x <- item
           if f x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

alphanum :: Parser Char
alphanum = sat isAlphaNum

leafLabel :: Parser (Tree String)
leafLabel = do xs <- some alphanum
               return (Leaf xs)

tree :: Parser (Tree String)
tree = do char '('
          l <- tree <|> leafLabel
          char ','
          r <- tree <|> leafLabel
          char ')'
          x <- some alphanum
          return (Node x l r)
       <|> leafLabel



