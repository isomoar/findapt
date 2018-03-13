{-# LANGUAGE OverloadedStrings #-}

module Telegram (sendLinks) where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import Data.Monoid ((<>))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import GetLinks

data Payload = Payload 
  { payloadText :: !T.Text }
  deriving Show

instance Aeson.ToJSON Payload where
  toJSON p = Aeson.object
    [ "text" .= payloadText p ]

mkPayload :: [Link] -> Payload
mkPayload links = Payload 
  { payloadText = T.unlines $ 
    [ "New links have been found. ", "" ]
    ++ ["- " <> lTitle link <> "\n " <> lHref link | link <- links]
  }

sendLinks :: [Link] -> IO ()
sendLinks links = do
  let token = Token "<token>" 
      chatId = ChatId 12345
      payload = mkPayload links
  manager <- newManager tlsManagerSettings
  result <- runTelegramClient token manager $ do
    sendMessageM (sendMessageRequest chatId $ 
      toStrict $ 
      toLazyText $
      Aeson.encodeToTextBuilder $ 
      Aeson.toJSON payload)
  print result
  print "done!"
