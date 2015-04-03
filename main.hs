module Main where

import Data.Char(isDigit,isAlpha)
import Control.Monad
import Test.HUnit

--------------------------------------------------------
-- Parser type
--------------------------------------------------------

data Parser a = Parser (String -> Maybe (String,a))

instance Monad Parser where
    return x = Parser (\s -> Just (s,x))
    (Parser p) >>= f = 
        Parser $ \s ->
            case p s of
                Nothing -> Nothing
                Just (s',x) -> let (Parser p') = f x in p' s' 

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of 
                                          Nothing -> Nothing
                                          Just(s,x) -> Just (s, f x)

instance MonadPlus Parser where
    mzero = Parser (\s -> Nothing)
    (Parser pa) `mplus` (Parser pb) = Parser $ \s ->
        case pa s of
            Nothing -> pb s
            x -> x

--------------------------------------------------------
-- Parser combinators
--------------------------------------------------------

match :: (Char -> Bool) -> Parser Char
match f = Parser $ \s ->
    case s of
      (x:xs) -> if f x then Just (xs, x) else Nothing
      _ -> Nothing

char :: Char -> Parser Char
char c = match (c==)

digit :: Parser Char
digit = match isDigit

int :: Parser Int
int = fmap read $ many1 digit

alphaNum :: Parser Char
alphaNum = match (\c -> isAlpha c || isDigit c)

space :: Parser Char
space = match (\c -> c == ' ' || c == '\t')

spaces :: Parser [Char]
spaces = many space
    
option :: a -> Parser a -> Parser a
option dx (Parser p) = Parser $ \s ->
    case p s of
        Just (s',x) -> Just (s',x)
        _ -> Just(s, dx)

many :: Parser a -> Parser [a]
many (Parser p) = fmap reverse $ Parser $ \s -> scan s []
    where 
        scan s xs = case p s of
                      Nothing -> Just (s, xs)
                      Just(s',x) -> scan s' (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- option [] (many1 p)
    return (x:xs)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = p1 `mplus` p2

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = do
    x <- p
    xs <- many (s >> p)
    return (x:xs)    

brackes :: Parser bl -> Parser a -> Parser br -> Parser a 
brackes pbl p pbr = do
    pbl >> spaces 
    x <- p
    spaces >> pbr
    return x

cbrackes :: Parser a -> Parser a
cbrackes p = brackes (char '{') p (char '}')

--------------------------------------------------------
-- Parser helpers
--------------------------------------------------------

parse :: Parser a -> String -> Maybe a
parse (Parser p) s = 
    case p s of
        Just (_,x) -> Just x
        _ -> Nothing

--------------------------------------------------------
-- Unit tests
--------------------------------------------------------

tests = test [
        "test 1" ~: parse (char 'a' >> char 'b' >> char 'c') "abc"                  @=? (Just 'c')                
    ,   "test 2" ~: parse (many digit >> char 'a')           "1234a"                @=? (Just 'a')                    
    ,   "test 3" ~: parse int                                "154"                  @=? (Just 154)                
    ,   "test 4" ~: parse (many (char 'a' <|> char 'b'))     "abababbac"            @=? (Just "abababba")         
    ,   "test 5" ~: parse (sepBy (many alphaNum) (char ',')) "a23x,2,ba"            @=? (Just ["a23x","2","ba"])  
    ,   "test 6" ~: parse (cbrackes (many1 alphaNum))        "{ dssaa }"            @=? (Just "dssaa")
    ,   "test 7" ~: parse p7                                 "f1a = { 10, 20, 30 }" @=? (Just ("f1a", [10,20,30]))
    ]
        where 
            p7 = do 
                    label <- many1 alphaNum
                    spaces
                    char '='
                    spaces
                    numbers <- cbrackes (sepBy (spaces>>int) (char ','))
                    return (label, numbers) 

main = runTestTT tests

