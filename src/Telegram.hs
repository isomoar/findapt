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

-- mkPayload :: [Link] -> Payload
-- mkPayload links = Payload 
--   { payloadText = T.unlines $ 
--     [ "New links have been found. ", "" ]
--     ++ ["- " <> lTitle link <> "\n " <> lHref link | link <- links]
--   }

mkPayload :: [Link] -> T.Text
mkPayload links = 
  T.unlines $ ["\n" <> lTitle link <> ", " <> lHref link | link <- links]

sendLinks :: [Link] -> Token -> IO ()
sendLinks links token = do
  let chatId = ChatId 75166061
      payload =  mkPayload links
  manager <- newManager tlsManagerSettings
  result <- runTelegramClient token manager $ do
    let request = (sendMessageRequest chatId payload) {
      message_parse_mode = Just Markdown }
    sendMessageM request
  print result
  print token
  print "done!"
