{-# LANGUAGE OverloadedStrings #-}

module GetLinks 
  (Link (..), getMatchingLinks)
    where

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

data Link = Link 
  { lTitle :: !T.Text
  , lHref :: !T.Text
  } deriving Show


getMatchingLinks :: [T.Text] -> T.Text -> IO [Link]
getMatchingLinks patterns uri = do
  req <- Http.parseRequest (T.unpack uri)
  lbs <- Http.getResponseBody <$> Http.httpLbs req
  let htmlPageAsText = T.unpack $ TL.toStrict $ TL.decodeUtf8 lbs
      e = "<tr class=trm_02>" :: String
      t = map parseAmount . sections (~== e) $ TagSoup.parseTags htmlPageAsText 
      parseAmount = filter isDigit .  innerText . drop 1 . take 2 . 
        dropWhile (~/= ("<td class=tdm_05>" :: String)) 

      -- tmp (TagSoup.TagOpen "a" args) = 
      --   lookup "href" args
      --   >>= (\h -> if take 30 h == "/workpage.php?page=variant&rv="
      --             then Just (drop 30 h) else Nothing)
      -- tmp (TagSoup.TagOpen "td" args) = 
      --   lookup "class" args
      --   >>=
      --     (\c -> if c == "tdm_05" then  )
      -- tmp _ = Nothing
  print t
  print (length t)
  return $ 
  --   makeAbsolute uri $
  --   matchingLinks patterns $
    extractLinks htmlPageAsText

extractLinks :: String -> [Link]
extractLinks = findByClassName . TagSoup.parseTags
  where
    e = "<td class=tdm_05>" :: String
    findByClassName tags = map f $ sections (~== e) tags
    f :: [Tag String] -> Link
    f ts = Link (T.empty) . T.pack . TagSoup.fromTagText . head $ ts

-- extractLinks :: T.Text -> [Link]
-- extractLinks = findLinks . TagSoup.parseTags
--   where
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

