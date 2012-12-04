-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Conal.Blogify
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Convert a gitit markdown page to html for my blog.
-- Various transformations on the way.
-- 
-- Based on <http://johnmacfarlane.net/pandoc/scripting.html>
--
-- Test with @blogify < test.md > test.md.html@
----------------------------------------------------------------------

module Conal.Blogify (transformDoc,rewrite,trimBlankRefs) where

import Data.Monoid (mempty)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf,isSuffixOf)

import Text.Pandoc

import qualified Network.Gitit.Plugin.FixSymbols     as Sym
import qualified Network.Gitit.Plugin.BirdtrackShift as Bird
import qualified Network.Gitit.Plugin.Comment        as Com
import qualified Network.Gitit.Plugin.Ordinal        as Ord

-- Steps:
-- 
-- * Indent lines starting with "<" unless followed by space.
--   Distinguishes inverse bird tracks from HTML.
-- * Remove gitit page meta-data if present (before parsing)
-- * Remove "Introduction" section header
-- * Deepen all headers by two levels
-- * Remove comments surrounded by <!--[ ... ]-->
-- * Fix source links: "src/" --> "/blog/src/"
-- * Rewrite some code symbols
-- * "<!-- references -->" and adjacent newlines from start of doc.


-- | Unary transformation
type Unop a = a -> a

tweakBlock :: Unop Block
tweakBlock (RawBlock "html" "<!-- references -->") = Null
tweakBlock (Header 1 [Str "Introduction"]) = Null
tweakBlock (Header n xs) = Header (n+2) xs
tweakBlock (RawBlock "html" s) | isPrefixOf "<!--[" s && isSuffixOf "]-->" s = Null
tweakBlock x = (Com.fixBlock . Sym.fixBlock mempty) x

-- TODO: extract subst-map from metadata and pass to Sym.fixBlock above
-- and Sym.fixInline below.

-- Link [Str foo] ("src/xxx",title) --> Link [Str foo] ("blog/src/xxx",title)

tweakInline :: Inline -> [Inline]
tweakInline (Link inlines (url,title)) | isPrefixOf "src/" url =
  [Link inlines ("/blog/" ++ url,title)]
tweakInline x = (Ord.fixInline . Com.fixInline . Sym.fixInline mempty) x

-- Note type type differences:
-- 
-- Com.fixInline :: Inline -> Inline
-- Sym.fixInline :: Subst -> Inline -> Inline
-- Ord.fixInline :: Inline -> [Inline]

transformDoc :: Unop Pandoc
transformDoc = bottomUp (concatMap tweakInline) . bottomUp tweakBlock

{- gitit meta-data header example:
---
tags: number
url: http://conal.net/blog/posts/adding-numbers/
...
-}

onLines :: Unop [String] -> Unop String
onLines h = unlines . h . lines

-- Drop metadata
dropMeta :: Unop [String]
dropMeta ("---":rest) = tail $ dropWhile (/= "...") rest
dropMeta ss = ss

-- Drop given prefix or yield original if no match.
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix p s = fromMaybe s (dropPrefix' p s)

-- Drop prefix and yield suffix, if prefix matched
dropPrefix' :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix' [] bs                     = Just bs
dropPrefix' _  []                     = Nothing
dropPrefix' (a:as) (b:bs) | a == b    = dropPrefix' as bs
                          | otherwise = Nothing

trimNewlines :: Unop String
trimNewlines (c:cs) | c `elem` nlChars = trimNewlines cs
trimNewlines s                         = s

nlChars :: String
nlChars = "\n\r"


trimBlankRefs :: Unop String
trimBlankRefs = trimNewlines
              . dropPrefix "<!-- references -->"
              . trimNewlines  

readDoc :: String -> Pandoc
readDoc = readMarkdown (defaultParserState {stateLiterateHaskell = True})

writeDoc :: Pandoc -> String
writeDoc = writeHtmlString (defaultWriterOptions { writerHTMLMathMethod = MathML Nothing })

-- There is another critically important step, which is to include the
-- contents of data/MathMLinHTML.js from Pandoc in my blog.
-- 
--   <script type="text/javascript" src=".../MathMLinHTML.js"></script>

rewrite :: Unop String
rewrite = trimBlankRefs
        . writeDoc
        . transformDoc
        . readDoc
        . onLines dropMeta
        . Bird.process

-- main :: IO ()
-- main = interact rewrite

