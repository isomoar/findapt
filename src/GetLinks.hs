{-# LANGUAGE OverloadedStrings #-}

module GetLinks 
  (Link (..), getMatchingLinks)
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
  ((~==), (~/=), fromTagText, sections, parseTags, Tag(..), innerText)
import System.IO

data Link = Link 
  { lTitle :: !T.Text
  , lHref :: !T.Text
  } deriving Show

(~===) a b = a ~== (b :: String)
(~/==) a b = a ~/= (b :: String)

getMatchingLinks :: [T.Text] -> T.Text -> IO [Link]
getMatchingLinks patterns uri = do
  -- lbs <- readFile "test.htm"
  -- let htmlPageAsText = lbs
  req <- Http.parseRequest (T.unpack uri)
  lbs <- Http.getResponseBody <$> Http.httpLbs req
  let htmlPageAsText = T.unpack $ TL.toStrict $ TL.decodeUtf8 lbs
      t =  map (parseRow . takeWhile (~/== "<div class=td-plata>")) 
            . sections (~=== "<tr class=trm_02>") $ 
              TagSoup.parseTags htmlPageAsText 
      parseRow (t:tags0) =
        let cond
              | TagSoup.tagOpenAttrLit "td" ("class", "tdm_05") t = 
                filter isDigit . innerText . take 2 $ tags0
              | TagSoup.tagOpenAttrLit "span" ("class", "tdm_rn") t = 
                concat . map fromTagText . take 1 $ tags0
              | otherwise = []
        in if cond == "" then parseRow tags0 else cond : parseRow tags0
      parseRow [] = []
  return $ map (\(am:d:_) -> Link (T.pack am) (T.pack d)) t

extractLinks :: String -> [Link]
extractLinks = findByClassName . TagSoup.parseTags
  where
    findByClassName tags = map f $ sections (~=== "<td class=tdm_05>") tags
    f :: [Tag String] -> Link
    f ts = Link (T.empty) . T.pack . TagSoup.fromTagText . head $ ts
    findLinks (TagSoup.TagOpen "a" args : tags0) = 
      let closeLink = (== TagSoup.TagClose "a") in
      case (lookup "href" args, break closeLink tags0) of
        (Nothing, (_, tags1)) -> findLinks tags1
        (_, (_, [])) -> []
        (Just href, (title, (_ : tags1))) -> 
          Link (TagSoup.innerText title) href : findLinks tags1
    findLinks (_ : tags0) = findLinks tags0
    findLinks [] = []

matchingLinks :: [T.Text] -> [Link] -> [Link]
matchingLinks patterns = filter $
  \l -> any (`T.isInfixOf` T.toLower (lTitle l))  lpatterns
    where
      lpatterns = map T.toLower patterns

