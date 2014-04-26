{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Text.XML.HXT.Core
import qualified Data.List.Split as L


data Item = Item {
    rank :: String
  , title :: String
  , domain :: String
  , href :: String
  -- , points :: String
  } deriving (Show)

data Item2 = Item2 {
  } deriving (Show)

items = (isElem >>> hasName "tr") `containing` tdRank
items2 = (isElem >>> hasName "tr") `containing` tdSubtext


-- gets next TR row after 1st one
nextSibling = addNav 
    >>> listA followingSiblingAxis
    >>> unlistA
    >>> remNav
    >>> isElem

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

main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> items >>> parsedItems1
  print links
  links2 <- runX $ doc //> items2 >>> listA parsedItems2
  mapM_ print links2
