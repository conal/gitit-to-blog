{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Conal Elliott 2010-2016
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
-- Test with @blogify < test.md > test.html@
----------------------------------------------------------------------

module Main where

import Data.Monoid (mempty)
import Control.Arrow (first,second,(***))
import Data.Maybe (fromMaybe,mapMaybe)
import Data.List (isPrefixOf,isSuffixOf)
import Data.Set (insert)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Data

import Text.Pandoc

import Options

import qualified Network.Gitit.Plugin.FixSymbols     as Sym
import qualified Network.Gitit.Plugin.BirdtrackShift as Bird
import qualified Network.Gitit.Plugin.Comment        as Com
import qualified Network.Gitit.Plugin.Ordinal        as Ord
import qualified Network.Gitit.Plugin.ReviveATX      as Atx
import qualified Network.Gitit.Plugin.ListNoPara     as LP

data BOptions = BOptions { optPrivate :: Bool }

instance Options BOptions where
  defineOptions = BOptions
                   <$> simpleOption "private" False "Whether to drop .private sections"

main :: IO ()
main = runCommand $ \ BOptions{..} _args -> do
         -- print optPrivate
         interact (rewrite optPrivate)

rewrite :: Bool -> Unop String
rewrite private =
    trimBlankRefs
  . writeDoc private
  . uncurry transformDoc
  . second (prepass . readDoc . unlines)
  . extractSubst
  . map fixAtx
  . lines
  . Bird.process
 where
   prepass = if private then dropPrivateBlocks else id

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

transformDoc :: Sym.Subst -> Unop Pandoc
transformDoc subst =
    bottomUp (concatMap Ord.fixInline)
  . bottomUp tweakInline . bottomUp tweakBlock
 where
   tweakBlock :: Unop Block
   tweakBlock (RawBlock "html" "<!-- references -->") = Null
   -- tweakBlock (Header 1 _ [Str "Introduction"]) = Null
   -- tweakBlock (Header n at xs) = Header (n+2) at xs
   tweakBlock (RawBlock "html" s) | isPrefixOf "<!--[" s && isSuffixOf "]-->" s = Null
   tweakBlock x = ( LP.fixBlock
                  . Atx.fixBlock . Com.fixBlock . Sym.fixBlock subst) x
   -- Link [Str foo] ("src/xxx",title) --> Link [Str foo] ("blog/src/xxx",title)
   tweakInline :: Unop Inline
   tweakInline (Link attr inlines (url,title)) | "src/" `isPrefixOf` url =
     Link attr inlines ("/blog/" ++ url,title)
   tweakInline x = (Com.fixInline . Sym.fixInline subst) x

-- When we see a header block with "private" class, drop it and all
-- following blocks until we see another header at the same level or
-- lower. For now, doesn't work with ATX-style blocks, because my
-- ReviveATX hack isn't smart enough. (It's a post-parse hack and the
-- "{.private}" attribute gets misparsed.)
dropPrivateBlocks :: Unop Pandoc
dropPrivateBlocks (Pandoc meta blocks) = Pandoc meta (drops blocks)
 where
   drops :: Unop [Block]
   drops (Header n (_,classes,_) _ : rest)
     | "private" `elem` classes = drops (dropWhile (not . headerAtMost n) rest)
   drops (block : rest) = block : drops rest
   drops [] = []

headerAtMost :: Int -> Block -> Bool
headerAtMost n (Header m _ _) = m <= n
headerAtMost _ _              = False

-- The attributes aren't appearing. I guess I need to enable Ext_header_attributes

-- [Header 2 ("journaling-and-sharing",["private"],[]) [Str "Journaling",Space,Str "and",Space,Str "sharing"]]

-- Note type type differences:
-- 
--   Com.fixInline :: Inline -> Inline
--   Sym.fixInline :: Subst -> Inline -> Inline
--   Ord.fixInline :: Inline -> [Inline]
-- 
-- I used to compose Ord.fixInline with the others, wrapping the the composition
-- in a bottomUp.concatMap. However, doing so messes up Sym in a way I don't
-- understand.

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

extractSubst :: [String] -> (Sym.Subst,[String])
extractSubst =
    first (Map.fromList . read . fromMaybe "[]" . Map.lookup "substMap")
  . captureMeta

captureMeta :: [String] -> (Map String String, [String])
captureMeta ("---":rest) = (toMetaMap *** tail) $ span (/= "...") rest
captureMeta ss = (mempty,ss)

toMetaMap :: [String] -> Map String String
toMetaMap = Map.fromList . mapMaybe parseMetaLine

parseMetaLine :: String -> Maybe (String,String)
parseMetaLine str | (key,':':val) <- break (== ':') str = Just (key,val)
                  | otherwise = Nothing

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

readerOptions :: ReaderOptions
readerOptions = def
  { readerExtensions = exts
  , readerSmart      = True
  -- , readerOldDashes  = True         -- double hyphen for emdash
  }
 where
   exts = -- insert Ext_header_attributes $
          insert Ext_literate_haskell $
          readerExtensions def

readDoc :: String -> Pandoc
readDoc = either (error . ("readDoc: readMarkdown failed: " ++) . show) id .
          readMarkdown readerOptions

htmlMath :: HTMLMathMethod
htmlMath = MathML Nothing -- LaTeXMathML Nothing

-- MathML & LaTeXMathML work great in Firefox but not Safari or Chrome.
-- I could try JSMath again

writeDoc :: Bool -> Pandoc -> String
writeDoc private =
  writeHtmlString $
    def { writerHTMLMathMethod = htmlMath
        , writerHighlight      = True
        -- , writerNumberSections  = True
        -- , writerTableOfContents = True
        , writerStandalone = True -- needed for TOC
        , writerTemplate = wTemplate private
        }

-- Without writerTemplate, I lose all of my output when writerStandalone = True.
wTemplate :: Bool -> String
wTemplate private = unlines
  [ 
    "<title>$title$</title>"
  , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"
  , "<script src=\"" ++ mathJaxUrl ++ "/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" type=\"text/javascript\"></script>"
  , "<link rel=\"stylesheet\" href=\"file:///Users/conal/Journals/Current/wikidata/static/css/custom.css\" media=\"all\" type=\"text/css\"/>"
  , "<style>blockquote { padding-top: 0em; }</style>"
  , "<style media=print>body { font-size:100%; }</style>"
  , "<body>"
  , "$body$"
  , "</body>"
  ]
 where
   mathJaxUrl | private   = "https://cdn.mathjax.org/mathjax/latest"
              | otherwise = "file:///Users/conal/Downloads/MathJax-master"

-- There is another critically important step, which is to include the
-- contents of data/MathMLinHTML.js from Pandoc in my blog.
-- 
--   <script type="text/javascript" src=".../MathMLinHTML.js"></script>
-- 
-- Instead, for now, I copy from the browser.

fixAtx :: Unop String
fixAtx (span (== '#') -> (length -> n,' ':rest)) | n > 0 = replicate n '=' ++ rest
fixAtx str = str
