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
    -- parseFile,
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

pattern Success :: (a, st) -> Either (ParseError, st) (a, st)
pattern Success result = Right result

pattern Failure :: (String, st) -> Either (ParseError, st) (a, st)
pattern Failure err = Left err


data ParserState s = ParserState
  { input :: s         -- The remaining input stream
  , pos   :: Int       -- The current position in the input
  } deriving (Show, Eq)


-- Create an initial state from an input stream.
mkInitialState :: s -> ParserState s
mkInitialState s = ParserState { input = s, pos = 0 }

-- a (Parser s a) is a parser that operates on an input/stream of type `s` and has a result type of `a`
-- so a (Parser String Int) would be a parser that parses a string and gives an Int in the result
-- newtype Parser s a = Parser {runParser :: s -> Either (ParseError, s) (a, s)}
newtype Parser s a = Parser {runParser :: ParserState s -> Either (ParseError, ParserState s) (a, ParserState s)}


parse :: Parser s a -> s -> Either (String, s) (a, s)
parse p s =
    case runParser p (mkInitialState s) of
      Success (a,   st) -> Success (a, input st)
      Failure (err, st) -> Failure (err, input st)

-- generic Stream class so you can Implement your own Instances for whatever type e.g. Text/ByteString
class (Eq (Elem s), Show (Elem s)) => Stream s where
    type Elem s :: Type
    -- Get the next item and the rest
    uncons :: s -> Maybe (Elem s, s)

    -- For efficiency
    lengthS :: s -> Int
    takeS :: Int -> s -> s
    dropS :: Int -> s -> s
    splitAtS :: Int -> s -> (s, s)

    -- Test for prefix
    isSeqPrefixOf :: [Elem s] -> s -> Bool

    -- Convert to string for error messages
    showInput :: s -> String

-- Constraint for Arbitrary Stream s with element type e (requires ConstraintKinds, TypeOperators)
type StreamOf s e = (Stream s, Elem s ~ e)

-- Stream instance for lists of tokens
instance (Eq a, Show a) => Stream [a] where
    type Elem [a] = a
    uncons [] = Nothing
    uncons (x : xs) = Just (x, xs)

    lengthS = length
    takeS = take
    dropS = drop
    splitAtS = splitAt

    isSeqPrefixOf = List.isPrefixOf
    showInput = show

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

instance MonadFail (Parser s) where
    fail msg = Parser $ \input -> Failure (msg, input)

-- slightly annoying Stream conditions because we use lengthS to improve errors
instance Alternative (Parser s) where
    empty = Parser $ \input ->
        Failure ("Empty parser", input)

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    p1 <|> p2 = Parser $ \st ->
        case runParser p1 st of
            Success result -> Success result
            Failure (err1, st1) ->
                case runParser p2 st of
                        Success result -> Success result
                        -- if both parsers fail take the error from the parser that consumed more
                        Failure (err2, st2) ->
                            case compare (pos st1) (pos st2) of
                                GT -> Failure (err1, st1)
                                EQ -> Failure (err1 ++ " or " ++ err2, st1)
                                LT -> Failure (err2, st2)


newtype Committed s a = Committed {unCommitted :: Parser s a}
newtype Cut s a = Cut {unCut :: Parser s a}

try' :: Either (Committed s a) (Parser s a) -> Parser s a
try' (Right p) = try p  -- The usual backtracking try.
try' (Left (Committed p)) = p  -- Strip the commit wrapper and don’t reset on failure.


-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \st ->
    case uncons (input st) of
        Nothing -> Failure ("Unexpected end of input", st)
        Just (t, rest) -> Success (t, st')
          where st' = st {input = rest, pos = pos st + 1 }

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

-- Parse anything that's not a particular token
notToken :: (Stream s) => Elem s -> Parser s (Elem s)
notToken t = satisfy (/= t) ("not " ++ show t)



-- -- Parse a sequence of tokens
-- tokens :: (Stream s) => [Elem s] -> Parser s [Elem s]
-- tokens ts = Parser $ \st ->
--     let inp = input st
--         st' =
--     in if ts `isSeqPrefixOf` inp
--         then Success (ts, dropS (length ts) inp)
--         else Failure ("Expected " ++ show ts, inp)

-- Parse a sequence of tokens
tokens :: (Stream s) => [Elem s] -> Parser s [Elem s]
tokens ts = Parser $ \st ->
  let inp = input st
      n   = lengthS ts
  in if ts `isSeqPrefixOf` inp
        then let rest   = dropS n inp
                 newPos = pos st + n
                 newSt  = st { input = rest, pos = newPos }
             in Success (ts, newSt)
        else Failure ("Expected " ++ show ts, st)


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

-- -- A committed choice combinator (or cut)
-- commit :: Parser s a -> Parser s a
-- commit p = Parser $ \input ->
--   case runParser p input of
--       Failure err -> err `seq` Failure err  -- The key is that input is not reset!
--       success -> success


-- modifies the error of a parser on failure using a function (modifyErr)
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

{-

-}

asdf open close n  = Parser $ \st ->
    let inp = (input st)
        a = runParser open  inp
        b = runParser close inp
    in case (a,b) of
        (Success

    case runParser open  of
        (


-- -- parse something between matching pairs
inMatching open close p = do
  _ <- open
  content <-
  _ <- close

  where
    bt = between open close

      stuff <- many  noneOf [open,close]
      

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
char :: (StreamOf s Char) => Char -> Parser s Char
char = token

-- string :: String -> Parser String String
string :: (StreamOf s Char) => String -> Parser s String
string = tokens

-- spaces :: Parser String String
spaces :: (StreamOf s Char) => Parser s String
spaces = many (char ' ')

-- whitespace :: Parser String String
whitespace :: (StreamOf s Char) => Parser s String
whitespace = many (satisfy isSpace "whitespace")

-- digit :: Parser String Char
digit :: (StreamOf s Char) => Parser s Char
digit = satisfy isDigit "digit"

-- letter :: Parser String Char
letter :: (StreamOf s Char) => Parser s Char
letter = satisfy isAlpha "letter"

-- alphaNum :: Parser String Char
alphaNum :: (StreamOf s Char) => Parser s Char
alphaNum = satisfy isAlphaNum "alphanumeric character"
