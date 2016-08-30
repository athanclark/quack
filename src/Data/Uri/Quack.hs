{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , FlexibleInstances
  #-}

module Data.Uri.Quack
  ( Parser
  , runParser
  , decodeUtf8Query
  , ParserError (..)
  , -- * Combining
    unlabeled
  , (.=)
  , overEquals
  , toMaybe
  , -- * Atoms
    fromUtf8
  , attoparsec
  , aeson
  ) where


import qualified Data.Attoparsec.Text as Atto  (Parser, parseOnly)
import qualified Data.Aeson           as Aeson (FromJSON, eitherDecode)

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Functor.Identity
import Data.String (IsString (fromString))
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import Network.HTTP.Types.URI (Query)



data ParserState = ParserState
  { parserStateToParse :: [(T.Text, Maybe T.Text)]
  } deriving (Show, Eq)


initParserState :: [(T.Text, Maybe T.Text)] -> ParserState
initParserState xs = ParserState
  { parserStateToParse = xs
  }


decodeUtf8Query :: Query -> [(T.Text, Maybe T.Text)]
decodeUtf8Query = map (\(l,r) -> (T.decodeUtf8 l, T.decodeUtf8 <$> r))


data ParserError
  = NoParse String
  deriving (Show, Eq)



newtype Parser a = Parser
  { getParser :: StateT ParserState (ExceptT ParserError Identity) a
  } deriving (Functor, Applicative, Monad)


runParser :: Parser a -> [(T.Text, Maybe T.Text)] -> Either ParserError a
runParser p xs = fst <$> runParser' p (initParserState xs)


runParser' :: Parser a -> ParserState -> Either ParserError (a, ParserState)
runParser' (Parser p) =
  runIdentity . runExceptT . runStateT p


instance Alternative Parser where
  empty = Parser $ StateT $ \_ -> throwError $ NoParse "empty"
  x <|> y = Parser $ do
    s <- get
    case runParser' x s of
      Right (x', s') -> do
        put s'
        pure x'
      Left _ ->
        case runParser' y s of
          Right (y', s') -> do
            put s'
            pure y'
          Left e -> throwError e
  some f = Parser $ do
    s <- get
    case runParser' f s of
      Right (x, s') -> do
        put s'
        xs <- getParser $ many f
        pure (x:xs)
      Left e -> throwError e
  many f = Parser $ do
    s <- get
    case runParser' f s of
      Right (x, s') -> do
        put s'
        xs <- getParser $ many f
        pure (x:xs)
      Left _ ->
        pure []

-- | Parse /only/ the label, disregarding any @=@ value;
--
-- > unlabeled (attoparsec double)
--
-- would parse something like
--
-- > "/foo?1234"
--
-- /or/
--
-- > "/foo?1234=asdf"
unlabeled :: PieceParser a -> Parser a
unlabeled (PieceParser f) = Parser $ do
  s <- get
  case parserStateToParse s of
    (x,_) : ss ->
      case f x of
        Left e -> throwError $ NoParse e
        Right y -> do put ParserState
                            { parserStateToParse = ss
                            }
                      pure y
    _ -> throwError $ NoParse "end of query"


-- | Parse with a label, but throw the parse result of the
--   label away;
--
-- > "foo" .= attoparsec double
--
-- would parse something like
--
-- > "/foo?foo=1234"
(.=) :: PieceParser T.Text -> PieceParser b -> Parser b
(PieceParser l) .= (PieceParser f) = Parser $ do
  s <- get
  case parserStateToParse s of
    (x,x') : ss ->
      case l x of
        Left e -> throwError $ NoParse e
        Right _ ->
          case x' of
            Nothing -> throwError $ NoParse "no assigned string"
            Just x'' ->
              case f x'' of
                Left e -> throwError $ NoParse e
                Right y -> do put ParserState
                                    { parserStateToParse = ss
                                    }
                              pure y
    _ -> throwError $ NoParse "end of query"


-- | @liftM2@ for parse results /between/ @=@, for instance:
--
-- > overEquals (,) (attoparsec $ double <* endOfInput)
-- >                (attoparsec $ double <* endOfInput)
--
-- would parse something like
--
-- > "/foo?1234=1234"
overEquals :: (a -> b -> c) -> PieceParser a -> PieceParser b -> Parser c
overEquals f (PieceParser l) (PieceParser r) = Parser $ do
  s <- get
  case parserStateToParse s of
    (x, x') : ss ->
      case l x of
        Left e -> throwError $ NoParse e
        Right l' ->
          case x' of
            Nothing -> throwError $ NoParse "no assigned string"
            Just x'' ->
              case r x'' of
                Left e -> throwError $ NoParse e
                Right r' -> do put ParserState
                                    { parserStateToParse = ss
                                    }
                               pure (f l' r')
    _ -> throwError $ NoParse "end of query"


toMaybe :: Parser a -> Parser (Maybe a)
toMaybe p = (Just <$> p) <|> pure Nothing


-- * Atoms

newtype PieceParser a = PieceParser (T.Text -> Either String a)

instance IsString (PieceParser T.Text) where
  fromString s = PieceParser $ \t ->
    if T.pack s == t
    then Right t
    else Left $ "Couldn't parse " ++ show t

fromUtf8 :: (T.Text -> Either String a) -> PieceParser a
fromUtf8 = PieceParser

attoparsec :: Atto.Parser a -> PieceParser a
attoparsec = fromUtf8 . Atto.parseOnly

aeson :: Aeson.FromJSON a => PieceParser a
aeson = fromUtf8 (Aeson.eitherDecode . LT.encodeUtf8 . LT.fromStrict)
