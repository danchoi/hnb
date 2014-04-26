{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Text.XML.HXT.Core
import Data.Char
import qualified Data.List.Split as L
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative

data Item = Item {
    rank :: String
  , title :: String
  , domain :: String
  , href :: String
  -- , points :: String
  } deriving (Show)

data Subtext = Subtext {
    points :: Int
  , user :: String
  , when :: Time
  , comments :: Int }
  | Other {
    when :: Time 
  } deriving (Show)

data Time = Time Int TimeUnit deriving (Show)

type TimeUnit = String

items = (isElem >>> hasName "tr") `containing` tdRank
items2 = (isElem >>> hasName "tr") `containing` tdSubtext


tdRank = (deep getChildren >>> hasName "td" >>> hasAttrValue "class" (== "title") )

tdSubtext = (deep getChildren >>> hasName "td" >>> hasAttrValue "class" (== "subtext") )

tdTitleNode = (getChildren >>> isElem >>> hasName "td") 

parsedItems1 = proc x -> do
    r <- tdRank >>> getChildren >>> getText -< x
    t <- tdTitleNode /> isElem >>> hasName "a" >>> getChildren >>> getText -< x
    d <- tdTitleNode /> isElem >>> hasName "span" >>> getChildren >>> getText -< x
    h <- tdTitleNode /> isElem >>> hasName "a" >>> getAttrValue "href" -< x
    -- p <- nextSibling >>> (deep getText) -< x
    returnA -< (r, t, d, h)

parsedItems2 = proc x -> do
    a <- tdSubtext //> getText -< x
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
    string " points by "
    user <- takeWhile1 (not.isSpace)
    space
    t <- timeUnit
    many space
    string "|" 
    many space
    c <- ("0" <$ string "discuss") <|> (takeWhile1 isDigit <* takeText) 
    return $ Subtext (read . T.unpack $ points) (T.unpack user) t (read . T.unpack $ c)

other :: Parser Subtext
other = do 
    t <- timeUnit
    return $ Other t

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



main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> items >>> parsedItems1
  -- print links
  links2 <- runX $ doc //> items2 >>> listA parsedItems2 >>> arr concat
  mapM_ print $ links2 
  mapM_ print $ map parseSubText links2 
  mapM_ print $ zip links ( map parseSubText links2 )


{-
http://hackage.haskell.org/package/attoparsec-0.11.2.1
-}
