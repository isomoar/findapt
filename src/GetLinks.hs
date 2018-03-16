{-# LANGUAGE OverloadedStrings #-}

module GetLinks 
  (Apartment (..), getMatchingLinks)
    where

import Data.List
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Char (isDigit)
import qualified Network.HTTP.Conduit as Http (parseUrl)
import qualified Network.HTTP.Simple as Http 
import qualified Network.URI as Uri 
import Text.HTML.TagSoup.Match as TagSoup
import Text.HTML.TagSoup as TagSoup
  ((~==), (~/=), fromTagText, fromAttrib, sections, parseTags, Tag(..), innerText)
import System.IO

data Apartment = Apartment 
  { aDistrict :: !T.Text
  , aPrice :: !T.Text
  , aHref :: !T.Text
  } deriving Show

(~===) a b = a ~== (b :: String)
(~/==) a b = a ~/= (b :: String)

initial = Apartment T.empty T.empty T.empty

getMatchingLinks :: [T.Text] -> T.Text -> IO [Apartment]
getMatchingLinks patterns uri = do
  -- lbs <- readFile "test.htm"
  -- let htmlPageAsText = lbs
  req <- Http.parseRequest (T.unpack uri)
  lbs <- Http.getResponseBody <$> Http.httpLbs req
  let htmlPageAsText = T.unpack $ TL.toStrict $ TL.decodeUtf8 lbs
      t = map (parseRow initial . takeWhile (~/== "<div class=td-download-pdf>")) 
            . sections (~=== "<tr class=trm_02>") $ 
              TagSoup.parseTags htmlPageAsText 
      parseRow acc (t:tags0) =
        let withId attrs = 
              case lookup "href" attrs of
                Nothing -> False
                Just href -> take 30 href == "/workpage.php?page=variant&rv="
            cond
              | TagSoup.tagOpenAttrLit "td" ("class", "tdm_05") t = 
                acc {aPrice = T.pack $ 
                        filter isDigit . innerText . take 2 $ tags0 
                    }
              | TagSoup.tagOpenAttrLit "span" ("class", "tdm_rn") t = 
                acc { aDistrict = T.pack $ 
                        concat . map fromTagText . take 1 $ tags0
                    }
              | TagSoup.tagOpenLit "a" withId t = 
                acc { aHref = T.pack $
                        (++) "http://arenda-piter.ru" $ fromAttrib "href" t
                    }
              | otherwise = acc
        in  parseRow cond tags0
      parseRow acc [] = acc
  return t

matchingLinks :: [T.Text] -> [Apartment] -> [Apartment]
matchingLinks patterns = filter $
  \l -> any (`T.isInfixOf` T.toLower (aDistrict l))  lpatterns
    where
      lpatterns = map T.toLower patterns

