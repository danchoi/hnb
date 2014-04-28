{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Main where
import Text.XML.HXT.Core
import Data.Char
import Data.List (isPrefixOf, intercalate)
import qualified Data.List.Split as L
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Text.Printf
import Text.HandsomeSoup

data Title = Title {
    rank :: Int
  , title :: String
  , domain :: String
  , href :: String
  } deriving (Show)

data Subtext = Subtext {
    points :: Int
  , user :: String
  , ago :: Time
  , comments :: Int
  , commentsUrl :: String 
  } 
  | OtherSubtext { ago :: Time } deriving (Show)

data Time = Time { amount :: Int, unit :: TimeUnit } deriving (Show)

type TimeUnit = String

items = (isElem >>> hasName "tr") `containing` tdRank
items2 = (isElem >>> hasName "tr") `containing` tdSubtext

tdRank = (deep getChildren >>> hasName "td" >>> hasAttrValue "class" (== "title") )

tdSubtext = (deep getChildren >>> hasName "td" >>> hasAttrValue "class" (== "subtext") )

tdTitleNode = (getChildren >>> isElem >>> hasName "td") 

{-
Two possibilities:

Normal:
<td class="title"><a href="http://storagemojo.com/2014/04/25/amazons-glacier-secret-bdxl/">Amazonâ€™s Glacier secret: BDXL?</a><span class="comhead"> (storagemojo.com) </span></td>

scribed:
<td class="title">
  <a href="https://www.mtgox.com/img/pdf/20140424_announce_qa_en.pdf">Mt. Gox commences bankruptcy proceedings</a> 
  [<a href="http://www.scribd.com/vacuum?url=https://www.mtgox.com/img/pdf/20140424_announce_qa_en.pdf">scribd</a>]
  <span class="comhead"> (mtgox.com) </span>
</td>

Domain:

  May have domain, may not if Ask HN:

<td class="title"><a href="http://chr13.com/2014/04/20/using-facebook-notes-to-ddos-any-website/?">Using Facebook Notes to DDoS any website</a><span class="comhead"> (chr13.com) </span></td>
<td class="title"><a href="item?id=7650916">Ask HN: How does your home office look like?</a></td>

-}


parseRank = getChildren >>> getText >>^ read . Prelude.takeWhile isDigit

parseDomain = (tdTitleNode >>> getChildren >>> isElem >>> hasName "span") 
      >>. Prelude.take 1 >>> getChildren >>> getText >>^ T.unpack . removeParens . T.strip . T.pack
   where removeParens = T.tail . T.init

fixLink s | "http" `isPrefixOf` s = s
          | otherwise           = "https://news.ycombinator.com/" ++ s
 
parsedItem1 = proc x -> do
    r <- (tdRank >>> parseRank) >>. Prelude.take 1 -< x
    t <- (tdTitleNode >>> getChildren >>> hasName "a") >>. Prelude.take 1 >>> getChildren >>> getText -< x
    d <- parseDomain `orElse` constA "-" -< x
    h <- (tdTitleNode >>> getChildren >>> hasName "a") >>. Prelude.take 1 >>> getAttrValue "href" >>^ fixLink -< x
    returnA -< Title r t d h

parsedItem2 = proc x -> do
    a <- tdSubtext //> (deep getText <+> deep (hasName "a" >>> hasAttrValue "href" ("item" `isPrefixOf`) >>> getAttrValue "href" >>^ (' ':)) ) -< x
    returnA -< a

-- parsec

{-
"53 points by clxl 6 hours ago  | 23 comments
"7 points by danso 1 hour ago  | discuss"
"18 minutes ago"
-}

subText :: Parser Subtext
subText = do
    points <- takeWhile1 isDigit
    string " points by " <|> string " point by "
    user <- takeWhile1 (not.isSpace)
    space
    t <- timeUnit
    many space
    string "|" 
    many space
    c <- (string "discuss" *> pure "0") <|> (takeWhile1 isDigit <* (string " comments" <|>  string " comment"))
    url <- space >> takeText 
    return $ Subtext (read . T.unpack $ points) (T.unpack user) t (read . T.unpack $ c) 
      (T.unpack url)

other :: Parser Subtext
other = do 
    t <- timeUnit
    return $ OtherSubtext t 

timeUnit :: Parser Time
timeUnit = do     
    d <- takeWhile1 isDigit
    space
    u <- takeWhile1 (not.isSpace) 
    string " ago"
    return $ Time (read . T.unpack $ d) (T.unpack u)


parseSubText s = case parseOnly (subText <|> other) (T.pack s) of
  Left err -> error $ "Failed to parse input " ++ err
  Right x -> x


header = intercalate "\t" ["#", "pts", "com", "title", "domain", "time", "href", "commentsUrl"]

lineFmt = "%d\t%d\t%d\t%s\t%s\t%s\t%s\t%s\n"

-- placeholder
ph :: String
ph = "-" 

printTitle (Title{..}, Subtext{..})      = printf lineFmt      rank points comments title domain (printTime ago) href commentsUrl 
printTitle (Title{..}, OtherSubtext{..}) = printf "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" rank ph ph title domain (printTime ago) href ph

printTime (Time a u) = show a ++ " " ++ u ++ " ago"

main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> items >>> parsedItem1
  -- mapM_ print links
  links2 <- runX $ doc //> items2 >>> listA parsedItem2 >>> arr concat
  -- mapM_ print $ zip links ( map parseSubText links2 )
  putStrLn header
  mapM_ printTitle $ zip links ( map parseSubText links2 )


{-
http://hackage.haskell.org/package/attoparsec-0.11.2.1
-}
