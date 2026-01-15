{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}


module SimpleGenericParser (
    -- Types
    Parser (..),
    -- ParserResult (..),
    -- Stream typeclass
    Stream (..),
    -- Running parsers
    parse,
    parseFile,
    -- Basic parsers
    anyToken,
    satisfy,
    token,
    notToken,
    tokens,
    oneOf,
    noneOf,
    -- Combinators
    try,
    optional,
    choice,
    between,
    sepBy,
    sepBy1,
    many,
    some,
    modifyError,
    -- Character parsers for String
    char,
    string,
    spaces,
    whitespace,
    digit,
    letter,
    alphaNum,
    -- Type aliases
    StreamOf,
) where


import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Kind (Type)
import qualified Data.List as List

type ParseError = String


-- I use these pattern synonyms so that things are clearer but you could just as easily use Either where Left is the Failure case and Right is the Success case
pattern Success :: (a, s) -> Either (ParseError, s) (a, s)
pattern Success result = Right result

pattern Failure :: (String, s) -> Either (ParseError, s) (a, s)
pattern Failure err = Left err

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

-- Constraint for Arbitrary Stream s with element type e (requires ConstraintKinds, TypeOperators)
type StreamOf e s = (Stream s, Elem s ~ e)

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
-- newtype Parser s a = Parser {runParser :: s -> ParserResult s (a, s)}
newtype Parser s a = Parser {runParser :: s -> Either (ParseError, s) (a, s)}

-- Run a parser
parse :: Parser s a -> s -> Either (ParseError, s) a
parse p input = case runParser p input of
    Success (result, _) -> Right result
    Failure err -> Left err

parseFile :: FilePath -> Parser String a -> IO (Either (ParseError, String) a)
parseFile filePath parser = do
    input <- readFile filePath
    return $ parse parser input

instance Functor (Parser s) where
    fmap f parser = Parser $ \input ->
        case runParser parser input of
            Success (v, rest) -> Success (f v, rest)
            Failure err -> Failure err
    -- -- alternate definition in terms of the monad instance
    -- fmap f parser = do
    --   v <- parser
    --   return (f v)

instance Applicative (Parser s) where
    pure x = Parser $ \input -> Success (x, input)

    pf <*> px = Parser $ \input ->
        case runParser pf input of
            Failure err -> Failure err
            Success (f, rest) ->
                case runParser px rest of
                    Failure err -> Failure err
                    Success (x, rest') -> Success (f x, rest')

    -- -- alternate definition in terms of the monad instance
    -- pf <*> px = do
    --   f <- pf
    --   x <- px
    --   return (f x)

instance Monad (Parser s) where
    -- monad instance essentially just handles the errors so if you chain a bunch of parser monadic actions if any of them fail, it'll short circuit and output whatever error
    -- 
    parser >>= f = Parser $ \input ->
        case runParser parser input of
            Failure err -> Failure err
            Success (v, rest) -> runParser (f v) rest

instance MonadFail (Parser s) where
    fail msg = Parser $ \input -> Failure (msg, input)

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
                        case compare (lengthS remaining1) (lengthS remaining2) of
                            LT -> Failure (err1, remaining1)
                            EQ -> Failure (err1 ++ " or " ++ err2, remaining1)
                            GT -> Failure (err2, remaining2)

    -- unnecessary implementations (haskell has equivalent default implementations based on <|> and empty)
    -- many p = Parser $ \input ->
    --     case runParser p input of
    --         Failure _ -> Success ([], input)
    --         Success (x, rest) ->
    --             case runParser (many p) rest of
    --                 Failure _ -> Success ([x], rest)
    --                 Success (xs, finalRest) -> Success (x : xs, finalRest)

    -- takes a parser p and returns another parser that tries p 1 or more times, collecting the results into a list upon successful parsing.
    some p = do
        x <- p          -- try to parse one item using parser p failing if it fails
        xs <- many p    -- parse as many results as you can using p
        pure (x : xs)   -- wrap the list of results in a parser

    -- same as some but upon failure just gives a parser with an empty list of results
    many :: Stream s => Parser s a -> Parser s [a]
    many p = some p <|> pure []


-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \input ->
    case uncons input of
        Nothing -> Failure ("Unexpected end of input", input)
        Just (t, rest) -> Success (t, rest)

-- Match a token that satisfies a predicate
satisfy :: (Stream s) => (Elem s -> Bool) -> String -> Parser s (Elem s)
satisfy pred expected = try $ do
    t <- anyToken `modifyError` \msg -> msg ++ ", Expected " ++ expected
    if pred t
        then return t
        else fail $ "Expected " ++ expected ++ ", found " ++ show t

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
-- oneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
oneOf :: (Stream s, Foldable t, Show (t (Elem s))) => t (Elem s) -> Parser s (Elem s)
oneOf ts = satisfy (`elem` ts) ("one of " ++ show ts)

-- Parse none of the tokens in the list
-- noneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
noneOf :: (Stream s, Foldable t, Show (t (Elem s))) => t (Elem s) -> Parser s (Elem s)
noneOf ts = satisfy (`notElem` ts) ("none of " ++ show ts)

-- tries a parser but on failure doesn't consume input
try :: Parser s a -> Parser s a
try p = Parser $ \input ->
    case runParser p input of
        Failure (msg, _) -> Failure (msg, input)
        success -> success

-- modifies the error of a parser on failure using a function
modifyError :: Parser s a -> (String -> String) -> Parser s a
modifyError parser modify = Parser $ \input ->
    case runParser parser input of
        Failure (msg, remaining) -> Failure (modify msg, remaining)
        success -> success

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

-- Character-specific parsers (for Char streams)
-- char :: Char -> Parser String Char
char :: (StreamOf Char s) => Char -> Parser s Char
char = token

-- string :: String -> Parser String String
string :: (StreamOf Char s) => String -> Parser s String
string = tokens

-- spaces :: Parser String String
spaces :: (StreamOf Char s) => Parser s String
spaces = many (char ' ')

-- whitespace :: Parser String String
whitespace :: (StreamOf Char s) => Parser s String
whitespace = many (satisfy isSpace "whitespace")

-- digit :: Parser String Char
digit :: (StreamOf Char s) => Parser s Char
digit = satisfy isDigit "digit"

-- letter :: Parser String Char
letter :: (StreamOf Char s) => Parser s Char
letter = satisfy isAlpha "letter"

-- alphaNum :: Parser String Char
alphaNum :: (StreamOf Char s) => Parser s Char
alphaNum = satisfy isAlphaNum "alphanumeric character"
