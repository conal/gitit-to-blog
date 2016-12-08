{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

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

-- -- Ordinal with superscript?
-- #define OrdinalSuper

module Main where

import Data.Monoid (mempty)
import Control.Arrow (first,second,(***),(&&&))
import Data.Maybe (fromMaybe,mapMaybe)
import Data.List (isPrefixOf,isSuffixOf)
import Data.Char (isSpace)
import Data.Set (insert)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)

import qualified Data.String.QQ as QQ
import Text.Pandoc

import Options

import qualified Network.Gitit.Plugin.FixSymbols     as Sym
import qualified Network.Gitit.Plugin.BirdtrackShift as Bird
import qualified Network.Gitit.Plugin.Comment        as Com
import qualified Network.Gitit.Plugin.ReviveATX      as Atx
import qualified Network.Gitit.Plugin.ListNoPara     as LP
#ifdef OrdinalSuper
import qualified Network.Gitit.Plugin.Ordinal        as Ord
#endif

data BOptions = BOptions { optPrivate :: Bool }

instance Options BOptions where
  defineOptions = BOptions
                   <$> simpleOption "private" False "Whether to drop .private sections"

main :: IO ()
main = runCommand $ \ BOptions{..} _args -> do
         -- print optPrivate
         interact (rewrite optPrivate)

rewrite :: Bool -> Unop String
#if 0
rewrite private str =
  let doc = readDoc str in
    -- show doc ++
    writeDoc private doc
#else
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
#endif

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
transformDoc subst doc@(Pandoc _meta _) =
  -- trace (show meta) $
#ifdef OrdinalSuper
  bottomUp (concatMap Ord.fixInline) .
#endif
  bottomUp tweakInline . bottomUp tweakBlock
  $ doc
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

fixAtx :: Unop String
fixAtx (span (== '#') -> (length -> n,' ':rest)) | n > 0 = replicate n '=' ++ rest
fixAtx str = str

onLines :: Unop [String] -> Unop String
onLines h = unlines . h . lines

-- -- Drop metadata
-- dropMeta :: Unop [String]
-- dropMeta ("---":rest) = tail $ dropWhile (/= "...") rest
-- dropMeta ss = ss

extractSubst :: [String] -> (Sym.Subst,[String])
extractSubst =
    first (Map.fromList . uncurry mappend . (oldStyle &&& newStyle))
  . captureMeta
 where
   newStyle = parseSubst . fromMaybe "[]" . Map.lookup "subst"
   oldStyle = read       . fromMaybe "[]" . Map.lookup "substMap"

parseSubst :: String -> [(String,String)]
parseSubst = map (second (dropWhile isSpace) . break isSpace) . read

-- str1 :: String
-- str1 = "[\"&&& △\",\"*** ×\",\"||| ▽\",\"+++ +\",\"|- ⊢\",\"<~ ⤺\",\"k (↝)\",\"op (⊙)\",\"--> ⇨\",\"+-> ➔\",\":*: ✖\",\":+: ➕\",\":->? ⤔\",\"Unit ()\",\"R ℝ\",\"Unit 𝟙\"]"

captureMeta :: [String] -> (Map String String, [String])
captureMeta ("---":rest) = (toMetaMap *** tail) $ break isMetaEnd rest
captureMeta ss           = (mempty,ss)

isMetaEnd :: String -> Bool
isMetaEnd = (`elem` ["...", "---"])

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
          insert Ext_emoji $  -- :smile:
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
    def { writerHTMLMathMethod    = MathJax mathJaxUrl
                                    -- LaTeXMathML (Just mathJaxUrl)
                                    -- htmlMath
        , writerHighlight         = True
        -- , writerNumberSections = True
        , writerTableOfContents   = True
        , writerTOCDepth          = 8
        , writerStandalone        = True -- needed for TOC
        , writerTemplate          = wTemplate private
        }
 where
   mathJaxUrl = mathJaxLoc ++ "/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
   mathJaxLoc | private   = "https://cdn.mathjax.org/mathjax/latest"
              | otherwise = "file:///Users/conal/Downloads/MathJax-master"

-- <script src="file:///Users/conal/Downloads/MathJax-master/MathJax.js?config=TeX-AMS-MML_HTMLorMML" type="text/javascript"></script>

-- Without writerTemplate, I lose all of my output when writerStandalone = True.
-- I got this content by running "pandoc -D html", and trimming
wTemplate :: Bool -> String
#if 1
wTemplate _private = [QQ.s|
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"$if(lang)$ lang="$lang$" xml:lang="$lang$"$endif$$if(dir)$ dir="$dir$"$endif$>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta http-equiv="Content-Style-Type" content="text/css" />
  <meta name="generator" content="pandoc" />
$for(author-meta)$
  <meta name="author" content="$author-meta$" />
$endfor$
$if(date-meta)$
  <meta name="date" content="$date-meta$" />
$endif$
$if(keywords)$
  <meta name="keywords" content="$for(keywords)$$keywords$$sep$, $endfor$" />
$endif$
  <title>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$</title>
  <style type="text/css">code{white-space: pre;}</style>
$if(quotes)$
  <style type="text/css">q { quotes: "“" "”" "‘" "’"; }</style>
$endif$
$if(highlighting-css)$
  <style type="text/css">
$highlighting-css$
  </style>
$endif$
<link rel="stylesheet" href="file:///Users/conal/Journals/Current/wikidata/static/css/custom.css" media="all" type="text/css"/>
$for(css)$
  <link rel="stylesheet" href="$css$" type="text/css" />
$endfor$
$if(math)$
  $math$
$endif$
$for(header-includes)$
  $header-includes$
$endfor$
</head>
<body>
$for(include-before)$
$include-before$
$endfor$
$if(title)$
<div id="$idprefix$header">
<h1 class="title">$title$</h1>
$if(subtitle)$
<h1 class="subtitle">$subtitle$</h1>
$endif$
$for(author)$
<h2 class="author">$author$</h2>
$endfor$
$if(date)$
<h3 class="date">$date$</h3>
$endif$
</div>
$endif$
$if(toc)$
<div id="$idprefix$TOC">
$toc$
</div>
$endif$
$body$
$for(include-after)$
$include-after$
$endfor$
</body>
</html>
|]
#else
wTemplate private = unlines
  [ "<head>"
  , "  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"
  , "  <script src=\"" ++ mathJaxUrl ++ "/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" type=\"text/javascript\"></script>"
  , "  <link rel=\"stylesheet\" href=\"file:///Users/conal/Journals/Current/wikidata/static/css/custom.css\" media=\"all\" type=\"text/css\"/>"
  , "  <style>blockquote { padding-top: 0em; }</style>"
  , "  <style media=print>body { font-size:70%; }</style>"

  , "  <script>MathJax.Hub.Config({tex2jax: {inlineMath: [['$$','$$'], ['\\\\(','\\\\)']], processEscapes: true}});</script>"

  -- , "$if(math)$ $math$ $endif$"
  , "<title>$title$</title>"
  , "</head>"
  , "<body>"

--   , "$if(title)$"
--   , "<div id=\"$idprefix$header\">"
--   , "<h1 class=\"title\">$title$</h1>"
--   , "$if(subtitle)$"
--   , "<h1 class=\"subtitle\">$subtitle$</h1>"
--   , "$endif$"
--   , "$for(author)$"
--   , "<h2 class=\"author\">$author$</h2>"
--   , "$endfor$"
--   , "$if(date)$"
--   , "<h3 class=\"date\">$date$</h3>"
--   , "$endif$"
--   , "</div>"
--   , "$endif$"

  , "$if(toc)$", "<div id=\"$idprefix$TOC\">", "$toc$", "</div>", "$endif$"
  , "$body$"
  , "</body>"
  ]
 where
   mathJaxUrl | private   = "https://cdn.mathjax.org/mathjax/latest"
              | otherwise = "file:///Users/conal/Downloads/MathJax-master"
#endif

-- There is another critically important step, which is to include the
-- contents of data/MathMLinHTML.js from Pandoc in my blog.
-- 
--   <script type="text/javascript" src=".../MathMLinHTML.js"></script>
-- 
-- Instead, for now, I copy from the browser.
