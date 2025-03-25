{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SimpleGenericParser (
    -- Types
    Parser (..),
    ParserResult (..),
    -- Stream typeclass
    Stream (..),
    -- Running parsers
    parse,
    parseFile,
    -- Basic parsers
    anyToken,
    satisfy,
    token,
    tokens,
    oneOf,
    noneOf,
    -- Combinators
    optional,
    choice,
    between,
    sepBy,
    sepBy1,
    many,
    some,
    -- Character parsers for String
    char,
    string,
    spaces,
    whitespace,
    digit,
    letter,
    alphaNum,
    -- Type aliases
    StringParser,
    StringParserResult,
) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Kind (Type)
import qualified Data.List as List

-- Result type parameterized by both input and result type
data ParserResult s a
    = Success a           -- a is (result,  unconsumed stream) from the parser definition
    | Failure (String, s) -- (ErrorMessage, Unconsumed stream)
    deriving (Show, Eq)

toEither :: ParserResult s a -> Either (String, s) a
toEither (Success a) = Right a
toEither (Failure e) = Left e

fromEither :: Either (String, s) a -> ParserResult s a
fromEither (Right a) = Success a
fromEither (Left e) = Failure e

-- Functor, Applicative, Monad instances
instance Functor (ParserResult s) where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

instance Applicative (ParserResult s) where
    pure = Success
    (Success f) <*> (Success a) = Success (f a)
    (Failure e) <*> _ = Failure e
    _ <*> (Failure e) = Failure e

instance Monad (ParserResult s) where
    (Success a) >>= f = f a
    (Failure e) >>= _ = Failure e


-- generic Stream class so you can Implement your own Instances for whatever type e.g. Text/ByteString
class (Eq (Elem s), Show (Elem s)) => Stream s where
    type Elem s :: Type
    -- Get the next item and the rest
    uncons :: s -> Maybe (Elem s, s)

    -- For efficiency
    takeS :: Int -> s -> s
    dropS :: Int -> s -> s
    lengthS :: s -> Int

    -- Test for prefix
    isSeqPrefixOf :: [Elem s] -> s -> Bool

    -- Convert to string for error messages
    showInput :: s -> String

-- Stream instance for lists of tokens
instance (Eq a, Show a) => Stream [a] where
    type Elem [a] = a
    uncons [] = Nothing
    uncons (x : xs) = Just (x, xs)

    takeS = take
    dropS = drop
    lengthS = length
    isSeqPrefixOf = List.isPrefixOf
    showInput = show

-- a (Parser s a) is a parser that operates on an input/stream of type `s` and has a result type of `a`
-- so a (Parser String Int) would be a parser that parses a string and gives an Int in the result
newtype Parser s a = Parser {runParser :: s -> ParserResult s (a, s)}

-- Run a parser
parse :: Parser s a -> s -> ParserResult s a
parse p input = case runParser p input of
    Success (result, _) -> Success result
    Failure err -> Failure err

parseFile :: FilePath -> Parser String a -> IO (ParserResult String a)
parseFile filePath parser = do
    input <- readFile filePath
    return $ parse parser input

instance Functor (Parser s) where
    fmap f parser = Parser $ \input ->
        case runParser parser input of
            Success (v, rest) -> Success (f v, rest)
            Failure err -> Failure err

instance Applicative (Parser s) where
    pure x = Parser $ \input -> Success (x, input)

    pf <*> px = Parser $ \input ->
        case runParser pf input of
            Failure err -> Failure err
            Success (f, rest) ->
                case runParser px rest of
                    Failure err -> Failure err
                    Success (x, rest') -> Success (f x, rest')

instance Monad (Parser s) where
    parser >>= f = Parser $ \input ->
        case runParser parser input of
            Failure err -> Failure err
            Success (v, rest) -> runParser (f v) rest

-- slightly annoying Stream conditions because we use lengthS to improve errors
instance (Stream s) => Alternative (Parser s) where
    empty = Parser $ \input ->
        Failure ("Empty parser", input)

    p1 <|> p2 = Parser $ \input ->
        case runParser p1 input of
            Success result -> Success result
            Failure (err1, remaining1) ->
                case runParser p2 input of
                    Success result -> Success result
                    -- if both parsers fail take the error from the parser that consumed more
                    Failure (err2, remaining2) ->
                        case compare (lengthS remaining2) (lengthS remaining2) of
                            LT -> Failure (err1, remaining1)
                            EQ -> Failure (err1 ++ " or " ++ err2, remaining1)
                            GT -> Failure (err2, remaining2)

    many p = Parser $ \input ->
        case runParser p input of
            Failure _ -> Success ([], input)
            Success (x, rest) ->
                case runParser (many p) rest of
                    Failure _ -> Success ([x], rest)
                    Success (xs, finalRest) -> Success (x : xs, finalRest)

    some p = do
        x <- p
        xs <- many p
        return (x : xs)

-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \input ->
    case uncons input of
        Nothing -> Failure ("Unexpected end of input", input)
        Just (t, rest) -> Success (t, rest)

-- Match a token that satisfies a predicate
satisfy :: (Stream s) => (Elem s -> Bool) -> String -> Parser s (Elem s)
satisfy pred expected = Parser $ \input ->
    case uncons input of
        Nothing -> Failure ("Unexpected end of input, expected " ++ expected, input)
        Just (t, rest) ->
            if pred t
                then Success (t, rest)
                else Failure ("Expected " ++ expected ++ ", found " ++ show t, input)

-- Parse a specific token
token :: (Stream s) => Elem s -> Parser s (Elem s)
token t = satisfy (== t) (show t)

notToken :: (Stream s) => Elem s -> Parser s (Elem s)
notToken t = satisfy (/= t) ("not " ++ show t)

-- Parse a sequence of tokens
tokens :: (Stream s) => [Elem s] -> Parser s [Elem s]
tokens ts = Parser $ \input ->
    if ts `isSeqPrefixOf` input
        then Success (ts, dropS (length ts) input)
        else Failure ("Expected " ++ show ts, input)

-- Parse one of the tokens in the list
oneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
oneOf ts = satisfy (`elem` ts) ("one of " ++ show ts)

-- Parse none of the tokens in the list
noneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
noneOf ts = satisfy (`notElem` ts) ("none of " ++ show ts)

-- Parse optional value
optional :: (Stream s) => Parser s a -> Parser s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- Parse one of a list of parsers
choice :: (Stream s) => [Parser s a] -> Parser s a
choice = asum
-- choice = foldr (<|>) empty

-- Parse something between delimiters
between :: Parser s open -> Parser s close -> Parser s a -> Parser s a
between open close p = open *> p <* close

-- Parse zero or more occurrences separated by delimiter
sepBy :: (Stream s) => Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- Parse one or more occurrences separated by delimiter
sepBy1 :: (Stream s) => Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = do
    x <- p
    xs <- many (sep *> p)
    return (x : xs)

-- Character-specific parsers (for String streams)
char :: Char -> Parser String Char
char = token

string :: String -> Parser String String
string = tokens

spaces :: Parser String String
spaces = many (char ' ')

whitespace :: Parser String String
whitespace = many (satisfy isSpace "whitespace")

digit :: Parser String Char
digit = satisfy isDigit "digit"

letter :: Parser String Char
letter = satisfy isAlpha "letter"

alphaNum :: Parser String Char
alphaNum = satisfy isAlphaNum "alphanumeric character"

-- Type aliases for common cases
type StringParser a = Parser String a
type StringParserResult a = ParserResult String a
