{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, state)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, stderr)
import Control.Monad (forever, unless)
import GetLinks
import Telegram
import Control.Exception
import System.Environment

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot (Token(..))

data Config = Config 
  { cPatterns :: [T.Text] 
  , cUrl :: T.Text
  , cFrom :: Integer
  , cTo :: Integer
  , token :: Token
  } deriving Show

type ApartmentSet = HS.HashSet T.Text

type WebWatchM = ReaderT Config (StateT ApartmentSet IO)

addLinks :: [Apartment] -> ApartmentSet -> ([Apartment], ApartmentSet)
addLinks links set = 
  (new, HS.union set (HS.fromList $ map aHref new))
    where
      new = filter (\l -> not $ aHref l `HS.member` set) links

webWatch :: Config -> IO ()
webWatch config = 
  evalStateT (runReaderT (forever watchOnce) config) HS.empty

-- apt = Apartment (T.pack "apt") (T.pack "1") (T.pack "2")

watchOnce :: WebWatchM ()
watchOnce = do
  Config {..} <- ask
  slog $ "Getting links from " ++ T.unpack cUrl
  links <- liftIO $ getMatchingLinks cPatterns cUrl
  -- slog $ "All links: " ++ show (map aHref links)
  newLinks <- state (addLinks links)
  slog $ "New links: " ++ show (map aHref newLinks)
  liftIO $ sendLinks newLinks token
  unless (null newLinks) $ do
    slog $ "Sending telegram message..."
    catchExceptions () $ sendLinks newLinks token
  slog $ "Sleeping 1 minute"
  liftIO $ threadDelay (5 * 1000 * 1000)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg

catchExceptions :: a -> IO a -> WebWatchM a
catchExceptions def action = do
  errOrX <- liftIO $ try action
  case errOrX of
    Right x -> return x
    Left se -> case fromException se of
                Just x -> liftIO $ throwIO (x :: AsyncException)
                Nothing -> do
                  slog $ "Error: " ++ show se
                  return def

parseConfig :: C.Config -> IO Config
parseConfig conf = do
  cPatterns <- C.require conf "patterns"
  cUrl <- C.require conf "url"
  cFrom <- C.require conf "from"
  cTo <- C.require conf "to"
  tokenEnv <- lookupEnv "TELEGRAM_TOKEN"
  let token = fromMaybe (Token T.empty) $ 
        (\x -> Token ("bot" <> T.pack x)) <$> tokenEnv
  return $ Config {..}

main :: IO ()
main = do
  config <- parseConfig =<< C.load [C.Required "config.conf"]
  webWatch config

