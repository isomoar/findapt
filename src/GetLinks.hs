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
  lbs <- readFile "test.htm"
  let htmlPageAsText = lbs
  -- req <- Http.parseRequest (T.unpack uri)
  -- lbs <- Http.getResponseBody <$> Http.httpLbs req
  -- let htmlPageAsText = T.unpack $ TL.toStrict $ TL.decodeUtf8 lbs
      t = map parseAmount . sections (~=== "<tr class=trm_02>") $ 
        TagSoup.parseTags htmlPageAsText 
      t1 = map parseDistrict . sections (~=== "<tr class=trm_02>") $ 
        TagSoup.parseTags htmlPageAsText 
      parseDistrict = map fromTagText . drop 1. take 2 . 
        dropWhile (~/== "<span class=tdm_rn>" ) 
      parseAmount = filter isDigit . innerText . take 5 . 
        dropWhile (~/== "<td class=tdm_05>" ) 
  putStr (intercalate ", "  t)
  putStr (intercalate ", " $ concat t1)
  -- print (t1)
  return []
  --   makeAbsolute uri $
  --   matchingLinks patterns $
    -- extractLinks htmlPageAsText

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

makeAbsolute :: T.Text -> [Link] -> [Link]
makeAbsolute url = mapMaybe $ \l -> do
  base <- baseUri
  href <- Uri.parseURI $ T.unpack (lHref l)
  return l {lHref = T.pack $ show $ href `Uri.relativeTo` base}
    where
      baseUri = Uri.parseURI $ T.unpack url

matchingLinks :: [T.Text] -> [Link] -> [Link]
matchingLinks patterns = filter $
  \l -> any (`T.isInfixOf` T.toLower (lTitle l))  lpatterns
    where
      lpatterns = map T.toLower patterns

