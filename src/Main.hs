{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified Data.HashSet as HS
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, state)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, stderr)
import Control.Monad (forever, unless)
import GetLinks
import Telegram
import Control.Exception

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

data Config = Config 
  { cPatterns :: [T.Text] 
  , cUrl :: T.Text
  } deriving Show

type LinkSet = HS.HashSet T.Text

type WebWatchM = ReaderT Config (StateT LinkSet IO)

addLinks :: [Link] -> LinkSet -> ([Link], LinkSet)
addLinks links set = 
  (new, HS.union set (HS.fromList $ map lHref new))
    where
      new = filter (\l -> not $ lHref l `HS.member` set) links

webWatch :: Config -> IO ()
webWatch config = 
  evalStateT (runReaderT (forever watchOnce) config) HS.empty

watchOnce :: WebWatchM ()
watchOnce = do
  Config cPatterns cUrl <- ask
  slog $ "Getting links from " ++ T.unpack cUrl
  links <- liftIO $ getMatchingLinks cPatterns cUrl
  slog $ "All links: " ++ show links
  newLinks <- state (addLinks links)
  slog $ "New links: " ++ show newLinks
  liftIO $ threadDelay (5 * 1000 * 1000)
  unless (null newLinks) $ do
    slog $ "Sending telegram message..."
    catchExceptions () $ sendLinks newLinks

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
  return $ Config cPatterns cUrl

main :: IO ()
main = do
  config <- parseConfig =<< C.load [C.Required "config.conf"]
  webWatch config

