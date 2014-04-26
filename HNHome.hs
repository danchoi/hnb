{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Main where
import Text.XML.HXT.Core
import Data.Char
import Data.List (isPrefixOf)
import qualified Data.List.Split as L
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Text.Printf

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

parsedItems1 = proc x -> do
    r <- tdRank >>> getChildren >>> getText >>^ read . Prelude.takeWhile isDigit -< x
    t <- tdTitleNode /> isElem >>> hasName "a" >>> getChildren >>> getText -< x
    d <- tdTitleNode /> isElem >>> hasName "span" >>> getChildren >>> getText >>^ T.unpack . T.strip . T.pack -< x
    h <- tdTitleNode /> isElem >>> hasName "a" >>> getAttrValue "href" -< x
    returnA -< Title r t d h

parsedItems2 = proc x -> do
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


printTitle (Title{..}, Subtext{..}) = printf "%d\t%d\t%s\t%s\t%d\t%s\t%s\t%s\n" rank points title domain comments (printTime ago) href commentsUrl 
printTitle (Title{..}, OtherSubtext{..}) = printf "%d\n" rank

printTime (Time a u) = show a ++ " " ++ u ++ " ago"

main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> items >>> parsedItems1
  links2 <- runX $ doc //> items2 >>> listA parsedItems2 >>> arr concat
  mapM_ printTitle $ zip links ( map parseSubText links2 )


{-
http://hackage.haskell.org/package/attoparsec-0.11.2.1
-}
