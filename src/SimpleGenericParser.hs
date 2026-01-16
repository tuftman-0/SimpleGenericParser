{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SimpleGenericParser
  ( -- Types
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
    wErrorHandler,
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
  )
where

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

-- a Parser is essentially a wrapper for a function that takes a "stream" of tokens and either fails with some error string or succeeds with some result
-- the point of the Parser wrapper is so we can implement instances of Functor, Applicative, Monad, Alternative for our new type
-- runParser is a function that "unwraps/runs a parser" 
-- a (Parser s a) is a parser that operates on an input/stream of type `s` and has a result type of `a`
-- so a (Parser String Int) would be a parser that parses a string and gives an Int in the result
newtype Parser s a = Parser {runParser :: s -> Either (ParseError, s) (a, s)}

-- Run a parser
parse :: Parser s a -> s -> Either (ParseError, s) a
parse p input = case runParser p input of
  Success (result, _) -> Right result
  Failure err -> Left err

parseFile :: FilePath -> Parser String a -> IO (Either (ParseError, String) a)
parseFile filePath parser = do
  input <- readFile filePath
  pure $ parse parser input


instance Functor (Parser s) where
  -- fmap f parser = Parser $ \input ->
  --   case runParser parser input of
  --     Success (v, rest) -> Success (f v, rest)
  --     Failure err -> Failure err

  -- alternate definition for fmap (<$>) in terms of the monad instance
  -- fmap :: (a -> b) -> Parser s a -> Parser s b
  -- fmap f parser = do
  --   value <- parser -- get value from running parser
  --   pure (f value)  -- apply function to value and re-wrap in parser
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f parser = parser >>= pure . f

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \input -> Success (x, input)

  -- pf <*> px = Parser $ \input ->
  --   case runParser pf input of
  --     Failure err -> Failure err
  --     Success (f, rest) ->
  --       case runParser px rest of
  --         Failure err -> Failure err
  --         Success (x, rest') -> Success (f x, rest')

  -- alternate definition of (<*>) in terms of the monad instance
  -- (much neater than the above definition because the failure cases are handled by the monad)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  pf <*> px = do
    f <- pf    -- get function from the first parser
    x <- px    -- get value from the second parser
    pure (f x) -- apply function to the value and re-wrap in parser

instance Monad (Parser s) where
  -- monad instance essentially just handles the errors so if you chain a bunch of parser actions if any of them fail, it'll short circuit and output whatever error
  -- makes a lot of things nicer because we can use do notation
  -- basically without the monad instance we'll have a lot of stuff like this
  -- pf <*> px = Parser $ \input ->
  --   case runParser pf input of
  --     Failure err -> Failure err
  --     Success (f, rest) ->
  --       case runParser px rest of
  --         Failure err -> Failure err
  --         Success (x, rest') -> Success (f x, rest')
  -- which can be converted to
  -- pf <*> px = do
  --   f <- pf    -- get function from the first parser (and fail if parsing fails)
  --   x <- px    -- get value from the second parser (and fail if parsing fails)
  --   pure (f x) -- apply function to the value and re-wrap in parser
  -- if we have a monad instance that handles failure
  -- this is what the do notation of the above function is converted to
  -- pf <*> px =
  --   pf >>= \f ->
  --     px >>= \x ->
  --       pure (f x)
  -- essentially monads let us chain actions more neatly
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  parser >>= f = Parser $ \input ->
    case runParser parser input of
      Failure err -> Failure err
      Success (v, rest) -> runParser (f v) rest

instance MonadFail (Parser s) where
  fail msg = Parser $ \input -> Failure (msg, input)

-- slightly annoying Stream conditions because we use lengthS to improve errors
instance (Stream s) => Alternative (Parser s) where
  empty :: Stream s => Parser s a
  empty = Parser $ \input -> Failure ("Empty parser", input)

  -- p1 <|> p2 = Parser $ \input ->
  --   case runParser p1 input of
  --     Success result -> Success result
  --     Failure (err1, remaining1) ->
  --       case runParser p2 input of
  --         Success result -> Success result
  --         -- if both parsers fail take the error from the parser that consumed more
  --         Failure (err2, remaining2) ->
  --           case compare (lengthS remaining1) (lengthS remaining2) of
  --             LT -> Failure (err1, remaining1)
  --             EQ -> Failure (err1 ++ " or " ++ err2, remaining1)
  --             GT -> Failure (err2, remaining2)

  -- tries to parse input using p1 but on failure it tries to parse using p2 and if both fail returns the error from the parser that consumed more input
  -- produces better errors because when chaining a bunch of parsers p1 <|> p2 <|> p3 <|> p4 and all of them fail you'll get a big unhelpful error
  -- this makes it so that if the parser that was "meant" to succeed fails we can see where it failed instead of seeing err1 or err2 or err3 or err4 which can be very long
  (<|>) :: Stream s => Parser s a -> Parser s a -> Parser s a
  p1 <|> p2 = do
    st0 <- getState
    p1 `onFailure` \(err1, st1) -> do
      putState st0
      p2 `onFailure` \(err2, st2) -> Parser $ \_ ->
        case compare (lengthS st1) (lengthS st2) of
          LT -> Failure (err1, st1)
          EQ -> Failure (err1 ++ " or " ++ err2, st1)
          GT -> Failure (err2, st2)

  -- unnecessary implementations (haskell has equivalent default implementations based on <|> and empty)
  -- takes a parser p and returns another parser that tries p 1 or more times, collecting the results into a list upon successful parsing.
  -- some p = do
  --   x <- p        -- try to parse one item using parser p (failing if it fails)
  --   xs <- many p  -- parse as many results as you can using p
  --   pure (x : xs) -- wrap the list of results in a parser

  -- this is equivalent to above
  -- essentially we're applying the cons operator (:) to the results of parsers (p) and (many p) but keeping it wrapped in a parser
  some :: Stream s => Parser s a -> Parser s [a]
  some p = (:) <$> p <*> many p

  -- same as some but upon failure just gives a parser with an empty list of results
  many :: (Stream s) => Parser s a -> Parser s [a]
  many p = some p <|> pure []

getState :: Parser s s
getState = Parser $ \st -> Success (st, st)

-- Unconditionally restore a saved state
putState :: s -> Parser s ()
putState st' = Parser $ \_ -> Success ((), st')

-- basically >>= but flips Success/Failure, passes successes through and focuses on errors
onFailure :: Parser s a -> ((ParseError, s) -> Parser s a) -> Parser s a
onFailure p handler = do
  st <- getState
  case runParser p st of
    Success res -> Parser $ \_ -> Success res
    Failure err -> handler err

-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \input ->
  case uncons input of
    Nothing -> Failure ("Unexpected end of input", input)
    Just (t, rest) -> Success (t, rest)

-- Match a token that satisfies a predicate
satisfy :: (Stream s) => (Elem s -> Bool) -> String -> Parser s (Elem s)
satisfy pred expected = try $ do
  t <- anyToken `wErrorHandler` \msg -> msg ++ ", Expected " ++ expected
  if pred t
    then pure t
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
modifyError :: (ParseError -> ParseError) -> Parser s a -> Parser s a
modifyError modify parser = parser `onFailure` \(err, st) -> Parser $ \_ -> Failure (modify err, st)

-- modifyError modify parser = Parser $ \input ->
--   case runParser parser input of
--     Failure (msg, remaining) -> Failure (modify msg, remaining)
--     success -> success

-- meant to be used as an infix version of modifyError
wErrorHandler :: Parser s a -> (ParseError -> ParseError) -> Parser s a
wErrorHandler = flip modifyError

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
  pure (x : xs)

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
