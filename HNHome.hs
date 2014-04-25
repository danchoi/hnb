{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Text.XML.HXT.Core


data Item = Item {
    rank :: String
  , title :: String
  , domain :: String
  , href :: String
  -- , points :: String
  } deriving (Show)

items = (isElem >>> hasName "tr") `containing` tdRank


-- gets next TR row after 1st one
nextSibling = addNav 
    >>> listA followingSiblingAxis
    >>> unlistA
    >>> remNav
    >>> isElem

tdRank = (deep getChildren >>> hasName "td" >>> hasAttrValue "class" (== "title") )

tdTitleNode = (getChildren >>> isElem >>> hasName "td") 


parsedItems = proc x -> do
    r <- tdRank >>> getChildren >>> getText -< x
    t <- tdTitleNode /> isElem >>> hasName "a" >>> getChildren >>> getText -< x
    d <- tdTitleNode /> isElem >>> hasName "span" >>> getChildren >>> getText -< x
    h <- tdTitleNode /> isElem >>> hasName "a" >>> getAttrValue "href" -< x
    -- p <- nextSibling >>> (deep getText) -< x
    returnA -< Item r t d h 


main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> items >>> parsedItems
  
  -- print links

  links2 <- runX $ doc //> hasName "table" >>> addNav 
    //> 
    withoutNav items  
    >>> listA followingSiblingAxis >>> arr head 
    >>> remNav
    

  print links2
