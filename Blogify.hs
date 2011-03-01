-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Blogify
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

module Main (main) where

import Data.List (isPrefixOf,isSuffixOf,isInfixOf)

import Text.Pandoc

import Paths_gitit_to_blog (getDataFileName)

import qualified Network.Gitit.Plugin.FixSymbols     as Sym
import qualified Network.Gitit.Plugin.BirdtrackShift as Bird

type Unop a = a -> a

-- * Indent lines starting with "<" unless followed by space.
--   Distinguishes inverse bird tracks from HTML.
-- * Remove gitit page meta-data if present (before parsing)
-- * Remove "Introduction" section header
-- * Remove "<!-- references -->"
-- * Deepen all headers by two levels
-- * Remove comments surrounded by <!--[ ... ]-->
-- * Fix source links: "src/" --> "/blog/src/"
-- * Rewrite some code symbols

tweakBlock :: Unop Block
tweakBlock (RawBlock "html" "<!-- references -->") = Null
tweakBlock (Header 1 [Str "Introduction"]) = Null
tweakBlock (Header n xs) = Header (n+2) xs
tweakBlock (RawBlock "html" s) | isPrefixOf "<!--[" s && isSuffixOf "]-->" s = Null
tweakBlock x = Sym.fixBlock x

-- TODO: handle comments via my gitit-comments plugin. Awkward now with
-- the types used there (Block -> [Block] and Inline -> [Inline]).

-- Link [Str foo] ("src/xxx",title) --> Link [Str foo] ("blog/src/xxx",title)

tweakInline :: Unop Inline
tweakInline (Link inlines (url,title)) | isPrefixOf "src/" url =
  Link inlines ("/blog/" ++ url,title)
tweakInline x = Sym.fixInline x

transformDoc :: Unop Pandoc
transformDoc = bottomUp tweakInline . bottomUp tweakBlock

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

readDoc :: String -> Pandoc
readDoc = readMarkdown (defaultParserState {stateLiterateHaskell = True})

writeDoc :: Pandoc -> String
-- writeDoc = writeMarkdown defaultWriterOptions
-- writeDoc = writeHtmlString defaultWriterOptions
writeDoc = writeHtmlString (defaultWriterOptions { writerHTMLMathMethod = MathML Nothing })

rewrite :: String -> Unop String
rewrite mathJS = mbMath mathJS
               . writeDoc
               . transformDoc
               . readDoc
               . onLines dropMeta
               . Bird.process

mbMath :: String -> Unop String
mbMath js html | isInfixOf "<math " html = html ++ wrapJS js
               | otherwise               = html

wrapJS :: Unop String
wrapJS = ("<script type=\"text/javascript\">" ++) . (++ "</script>")

main :: IO ()
main = getDataFileName "data/MathMLinHTML.js" >>= readFile >>= interact . rewrite

