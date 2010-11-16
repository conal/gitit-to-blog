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

import Data.List (isPrefixOf)

import Text.Pandoc

import qualified Network.Gitit.Plugin.FixSymbols as Sym

type Unop a = a -> a

-- * Remove gitit page meta-data if present (before parsing)
-- * Deepen all headers by two levels
-- * Fix source links: "src/" --> "/blog/src/"
-- * Rewrite some code symbols

tweakBlock :: Unop Block
tweakBlock (Header n xs) = Header (n+2) xs
tweakBlock x = Sym.fixBlock x

-- Link [Str foo] ("src/xxx",title) --> Link [Str foo] ("blog/src/xxx",title)

tweakInline :: Unop Inline
tweakInline (Link inlines (url,title)) | isPrefixOf "src/" url =
  Link inlines ("/blog/" ++ url,title)
tweakInline x = Sym.fixInline x

transformDoc :: Unop Pandoc
transformDoc = processWith tweakInline . processWith tweakBlock

{- gitit meta-data header example:
---
tags: number
url: http://conal.net/blog/posts/adding-numbers/
...
-}

onLines :: Unop [String] -> Unop String
onLines h = unlines . h . lines

dropMeta :: Unop [String]
dropMeta ("---":rest) = tail $ dropWhile (/= "...") rest
dropMeta ss = ss

readDoc :: String -> Pandoc
readDoc = readMarkdown (defaultParserState {stateLiterateHaskell = True})

writeDoc :: Pandoc -> String
-- writeDoc = writeMarkdown defaultWriterOptions
writeDoc = writeHtmlString defaultWriterOptions

main :: IO ()
main = interact (writeDoc . transformDoc . readDoc . onLines dropMeta)
