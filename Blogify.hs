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
-- Patterned off of <http://johnmacfarlane.net/pandoc/scripting.html>
----------------------------------------------------------------------

module Main (main) where

import Data.List (isPrefixOf)

import Text.Pandoc

import qualified Network.Gitit.Plugin.FixSymbols as Sym


-- * Deepen all headers by two levels
-- * Fix source links: "src/" --> "/blog/src/"
-- * Rewrite some code symbols

tweakBlock :: Block -> Block
tweakBlock (Header n xs) = Header (n+2) xs
tweakBlock x = Sym.fixBlock x

-- Link [Str foo] ("src/xxx",title) --> Link [Str foo] ("blog/src/xxx",title)

tweakInline :: Inline -> Inline
tweakInline (Link inlines (url,title)) | isPrefixOf "src/" url =
  Link inlines ("/blog/" ++ url,title)
tweakInline x = Sym.fixInline x

transformDoc :: Pandoc -> Pandoc
transformDoc = processWith tweakInline . processWith tweakBlock

readDoc :: String -> Pandoc
readDoc = readMarkdown (defaultParserState {stateLiterateHaskell = True})

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown defaultWriterOptions

main :: IO ()
main = interact (writeDoc . transformDoc . readDoc)
