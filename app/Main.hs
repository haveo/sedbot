{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Class                ( MonadState(get, put) )
import Data.ByteString                          ( ByteString )
import Data.Map.Strict qualified               as M
import Data.Map.Strict                          ( Map )
import Data.Maybe                               ( fromMaybe )
import Data.Sequence qualified                 as S
import Data.Sequence                            ( Seq )
import Data.Sized qualified                    as SZ
import Data.Sized                               ( Sized
                                                , SomeSized(..)
                                                , toSomeSized
                                                )
import Data.Text                               as T
import Data.Type.Natural                 hiding ( (%~) )
import Data.Type.Natural.Lemma.Order            ( leqRefl
                                                , leqSuccStepR
                                                )
import Data.Type.Ordinal                 hiding ( pattern OZ )
import Data.Vector                              ( Vector )
import Data.Vector.Unboxed qualified           as VU
import Data.Vector.Unboxed                      ( Unbox )
import Network.IRC.Client                hiding ( get )
import Numeric.Natural                          ( Natural )
import Options.Applicative
import Proof.Propositional                      ( withWitness )
import Text.Parsec                       hiding ( (<|>)
                                                , option
                                                )

data CacheMsg = CacheMsg Text Text
  deriving Show

data Command = Replace Text Text
  deriving Show

type MyState = Map Text (Seq CacheMsg)

pattern OZ :: forall n . () => (0 < n) => Ordinal n
pattern OZ = OLt Zero

distance :: Text -> Text -> Natural
distance s1 s2 = case (toSomeSized v1, toSomeSized v2) of
  (SomeSized sn xs, SomeSized sm ys) ->
    withKnownNat sn $ withKnownNat sm $ distance' xs ys
 where
  v1 = VU.fromList . T.unpack $ s1
  v2 = VU.fromList . T.unpack $ s2

distance'
  :: forall (n :: Nat) (m :: Nat) a
   . (Eq a, Unbox a, KnownNat n, KnownNat m)
  => Sized VU.Vector n a
  -> Sized VU.Vector m a
  -> Natural
distance' s1 s2 = go (lastOrd @n) (lastOrd @m)
 where
  lastOrd :: forall (k :: Nat) . (KnownNat k) => Ordinal (S k)
  lastOrd = withWitness (leqRefl sk) $ sNatToOrd' (Succ sk) sk
    where sk = sNat @k
  memo :: Sized Vector (S n) (Sized Vector (S m) Natural)
  memo = SZ.generate (Succ $ sNat @n) (SZ.generate (Succ $ sNat @m) . go)
  go :: Ordinal (S n) -> Ordinal (S m) -> Natural
  go i j = case (i, j) of
    (OZ   , _    ) -> ordToNatural j
    (_    , OZ   ) -> ordToNatural i
    (OS i', OS j') -> if s1 SZ.%!! i' == s2 SZ.%!! j'
      then memo SZ.%!! inc i' SZ.%!! inc j'
      else 1 + min
        (min (memo SZ.%!! inc i' SZ.%!! j) (memo SZ.%!! i SZ.%!! inc j'))
        (memo SZ.%!! inc i' SZ.%!! inc j')
    _ -> error "cant happen"
  inc :: forall (k :: Nat) . (KnownNat k) => Ordinal k -> Ordinal (S k)
  inc ord = withWitness (leqSuccStepR sk sk $ leqRefl sk) $ inclusion ord
    where sk = sNat @k

cacheMsg :: MonadState MyState m => Text -> Text -> Text -> m ()
cacheMsg chan name msg = get >>= put . M.alter act chan
  where act = Just . S.take 100 . (CacheMsg name msg S.<|) . fromMaybe mempty

filterChanMsg :: Event Text -> Maybe (Text, Text, Text)
filterChanMsg event = (,,) <$> chan <*> name <*> msg
 where
  msg  = preview (message . _Privmsg . _2 . _Right) event
  chan = preview (source . _Channel . _1) event
  name = preview (source . _Channel . _2) event

replaceOne :: Text -> Text -> Text -> Text
replaceOne p substitution text
  | T.null back = text
  | otherwise   = T.concat [front, substitution, T.drop (T.length p) back]
  where (front, back) = T.breakOn p text

sedHandler :: EventHandler MyState
sedHandler = EventHandler filterChanMsg $ \src (chan, name, msg) -> do
  let parseExact   = parseSed (char '/')
      parseLenient = parseSed (oneOf ['/', '\\'])
  case parse (try parseExact <|> parseLenient) "" msg of
    Left  _                  -> cacheMsg chan name msg
    Right (Replace p output) -> do
      hist <- M.lookup chan <$> get
      let resp = do
            h              <- hist
            CacheMsg n txt <-
              S.dropWhileL (\(CacheMsg _ s) -> T.count p s == 0) h S.!? 0
            let newTxt = replaceOne p output txt
            return . replyTo @MyState src $ if distance newTxt txt > 20
              then "I'm afraid I can't do that."
              else T.concat ["<", n, "> ", replaceOne p output txt]
      fromMaybe (pure ()) resp

parseSed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m Command
parseSed sep = do
  _      <- char 's'
  _      <- sep
  p      <- (pure <$> anyChar) <> manyTill anyChar sep
  output <- manyTill anyChar $ (() <$ sep) <|> eof
  eof
  return $ Replace (T.pack p) (T.pack output)

data Cfg = Cfg ByteString Int Text

run :: Cfg -> IO ()
run (Cfg host runPort name) = do
  let tlsConfig = WithDefaultConfig host runPort
  let conn :: ConnectionConfig MyState
      conn =
        tlsConnection tlsConfig
          & (username .~ name)
          & (realname .~ name)
          & (logfunc .~ stdoutLogger)
  let cfg =
        defaultInstanceConfig name
          & (handlers %~ ([sedHandler] ++))
          & (channels %~ (["haveo"] <>))
  st <- newIRCState conn cfg mempty
  runClientWith st

cfgParser :: Parser Cfg
cfgParser =
  Cfg
    <$> strOption
          (  long "host"
          <> short 'h'
          <> help "Hostname of the IRC server"
          <> metavar "HOST"
          )
    <*> option
          auto
          (long "port" <> short 'p' <> help "Port of the IRC server" <> metavar
            "PORT"
          )
    <*> strOption
          (long "name" <> short 'n' <> help "Nickname of the bot" <> metavar
            "NAME"
          )


opts :: ParserInfo Cfg
opts = info
  (cfgParser <**> helper)
  (  fullDesc
  <> progDesc "An IRC bot to correct previous messages using sed-like syntax"
  <> header "sedbot - IRC substitution bot"
  )

main :: IO ()
main = execParser opts >>= run
