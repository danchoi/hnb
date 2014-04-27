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

data Comment = Comment {
    nesting :: Int
  , text :: String
  , author :: String
  , timestamp :: String
  } deriving (Show)

items = css "table table table"

indentImg = deep ( hasName "img" >>> hasAttrValue "src" (== "s.gif"))

{-
parseTimestamp s = case parseOnly (subText <|> other) (T.pack s) of
  Left err -> error $ "Failed to parse input " ++ err
  Right x -> x
  -}


item = proc x -> do
    n <- indentImg >>> getAttrValue "width" >>^ read . Prelude.takeWhile isDigit -< x
    t <- listA (css "td.default span.comment" >>> deep getText) >>> arr concat -< x
            -- listA (deep (getChildren >>> hasAttrValue "class" (== "comment") >>> deep getText)) >>> arr concat   -< x
    a <- css "span.comhead" >>> css "a:first-child" >>> getChildren >>> getText -< x
    ts <- listA (css "span.comhead" >>> deep (getChildren >>> getText)) >>> arr concat -< x
    returnA -< Comment n t a ts

{-
        <tr>
          <td><img src="s.gif" height="1" width="0"></td>
          <td valign="top">
            ... IGNORE
          </td>
          <td class="default">
            <div style="margin-top:2px; margin-bottom:-10px;">
              <span class="comhead"><a href="user?id=neilk">neilk</a> 4 hours ago | <a href="item?id=7654207">link</a></span>
            </div><br>
            <span class="comment"><font color="#000000">The argument has an emotional resonance, but does it really make sense?
            It's not like working for big employers (and entrusting Wall Street with one's retirement) worked out really well in
            the past decade either.</font></span>


-}

main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  xs <- runX $ doc >>> items  >>> item
  -- xs <- runX $ doc >>> items 
  mapM_ print xs

