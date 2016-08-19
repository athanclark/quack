{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , FlexibleInstances
  #-}

module Data.Uri.Query where


import qualified Data.Attoparsec.Text as Atto  (Parser, parseOnly)
import qualified Data.Aeson           as Aeson (FromJSON, decode)
import Network.HTTP.Types.URI

import Data.ByteString as BS
import Data.Text       as T
import Data.Text.Encoding as T
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as LT
import Data.Functor.Identity
import Data.String (IsString (fromString))
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Error (hush)



data ParserState = ParserState
  { parserStateSoFar   :: [(T.Text, Maybe T.Text)]
  , parserStateToParse :: [(T.Text, Maybe T.Text)]
  } deriving (Show, Eq)


initParserState :: [(T.Text, Maybe T.Text)] -> ParserState
initParserState xs = ParserState
  { parserStateSoFar = []
  , parserStateToParse = xs
  }


data ParserError
  = NoParse
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
  empty = Parser $ StateT $ \_ -> throwError NoParse
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


unlabeled :: PieceParser a -> Parser a
unlabeled (PieceParser f) = Parser $ do
  s <- get
  case parserStateToParse s of
    (x,x') : ss ->
      case f x of
        Nothing -> throwError NoParse
        Just y  -> do put ParserState
                            { parserStateSoFar   = (x,x') : parserStateSoFar s
                            , parserStateToParse = ss
                            }
                      pure y
    _ -> throwError NoParse


-- | Useless label
(.=) :: PieceParser a -> PieceParser b -> Parser b
(PieceParser l) .= (PieceParser f) = Parser $ do
  s <- get
  case parserStateToParse s of
    (x,x') : ss ->
      case l x of
        Nothing -> throwError NoParse
        Just _  ->
          case f =<< x' of
            Nothing -> throwError NoParse
            Just y  -> do put ParserState
                                { parserStateSoFar   = (x,x') : parserStateSoFar s
                                , parserStateToParse = ss
                                }
                          pure y
    _ -> throwError NoParse


overEquals :: (a -> b -> c) -> PieceParser a -> PieceParser b -> Parser c
overEquals f (PieceParser l) (PieceParser r) = Parser $ do
  s <- get
  case parserStateToParse s of
    (x,x') : ss ->
      case l x of
        Nothing -> throwError NoParse
        Just l' ->
          case r =<< x' of
            Nothing -> throwError NoParse
            Just r' -> do put ParserState
                                { parserStateSoFar   = (x,x') : parserStateSoFar s
                                , parserStateToParse = ss
                                }
                          pure (f l' r')
    _ -> throwError NoParse



-- * Pieces

newtype PieceParser a = PieceParser
  { getPieceParser :: T.Text -> Maybe a
  }

instance IsString (PieceParser T.Text) where
  fromString s = PieceParser $ \t -> t <$ guard (T.pack s == t)

fromUtf8 :: (T.Text -> Maybe a) -> PieceParser a
fromUtf8 = PieceParser

attoparsec :: Atto.Parser a -> PieceParser a
attoparsec p = fromUtf8 (hush . Atto.parseOnly p)

aeson :: Aeson.FromJSON a => PieceParser a
aeson = fromUtf8 (Aeson.decode . LT.encodeUtf8 . LT.fromStrict)
