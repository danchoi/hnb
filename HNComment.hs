{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Main where
import Text.XML.HXT.Core
import Data.Char
import Data.List (isPrefixOf, intercalate, isInfixOf, sortBy)
import Data.Ord (comparing)
import qualified Data.List.Split as L
import qualified Data.Text as T
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Atto
import Control.Applicative
import Text.Printf
import Text.HandsomeSoup

data Top = Top {
    title :: String
  , body :: [String]
} deriving (Show)

data Comment = Comment {
    nesting :: Int
  , text :: [String]
  , author :: String
  , timestamp :: String
  } deriving (Show)

{-
top = proc x -> do
    t <- css "head title" >>> getText -< x
    returnA -< Top title []
-}
comments = css "table table table"

indentImg = deep ( hasName "img" >>> hasAttrValue "src" (== "s.gif"))

parseTimestamp s = case parseOnly ptimestamp (T.pack s) of
    Left err -> error $ "Failed to parse input " ++ err
    Right x -> x

ptimestamp :: Parser String
ptimestamp = do 
    many space
    x <- Atto.takeWhile (`notElem` ['|'])
    takeText
    return . T.unpack . T.strip $ x

itemPara = proc x -> do
    text <- listA (deep getText) >>> arr concat -< x
    returnA -< text

item = proc x -> do
    n <- indentImg >>> getAttrValue "width" >>^ read . Prelude.takeWhile isDigit -< x

    -- t <- listA (css "td.default span.comment" >>> itemPara)  >>^ map (T.pack . concat) -< x

    -- t <- listA (css "td.default span.comment" >>> itemPara)  >>^ map (T.pack . show) -< x

    t <- (css "td.default span.comment" >>> (listA itemPara))  >. concat -< x

    a <- css "span.comhead" >>> css "a:first-child" >>> getChildren >>> getText -< x

    ts <- listA (css "span.comhead" >>> getChildren >>> getText) >>> arr concat >>^ parseTimestamp -< x
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

printComment :: Comment -> IO () 
printComment Comment{..} = 
    let ps = map (pad nesting) text
        pad n s = (Prelude.take (n `div` 10) $ repeat ' ') ++ s
    in do 
      putStrLn $ intercalate "\n\n" ps
      putStrLn ""
      putStrLn $ pad nesting $ author ++ " " ++ timestamp
      putStrLn "\n"

main = do 
  html <- getContents 
  let doc = readString [withParseHTML yes, withWarnings no] html
  postTitle <- runX $ doc >>> css "body table:first-child td.title" >>> deep getText
  link <- runX $ doc >>> css "body table:first-child td.title a" >>> getAttrValue "href"
  subtext <- runX $ doc >>> css "body table:first-child td.subtext" >>> deep getText
  -- print postTitle 
  putStrLn $ intercalate " "  postTitle
  putStr "\n"
  putStrLn $ intercalate " " link
  putStr "\n"
  putStrLn $ intercalate ""  subtext
  putStr "\n"

  -- gets title then optionally paragraphs of post as array of paragraphs:
  -- e.g. ["Ask HN: What is the most difficult tech/dev challenge you ever solved?","I feel I just make some CRUDs. It's fine, since they useful for my customers. But they are not technical challenges. So please tell me yours."]

  postBody <- runX $ doc >>> css "body table:first-child table:first-child " >>> deep getText >>. (filter ((> 25) . length))
  putStrLn $ intercalate "\n\n"  postBody
  putStrLn "\n---\n"
  xs <- runX $ doc >>> comments  >>> item
  mapM_ printComment xs

