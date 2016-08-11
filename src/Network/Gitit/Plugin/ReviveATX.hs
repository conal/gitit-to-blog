{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Network.Gitit.Plugin.ReviveATX
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Gitit plugin: Format ordinals like 21st
----------------------------------------------------------------------

-- #define Testing

module Network.Gitit.Plugin.ReviveATX (plugin, fixBlock) where

import Network.Gitit.Interface

#ifdef Testing
-- TEMP. Also remove data-default-class in .cabal
import Data.Default.Class
import Text.Pandoc  -- also temp
#endif

plugin :: Plugin
plugin = PageTransform $ return . bottomUp fixBlock

-- Convert
--   [Para [Str "###",Space,Str "Deeper"]]
-- into
--   [Header 3 ("deeper",[],[]) [Str "Deeper"]]

fixBlock :: Block -> Block
fixBlock (Para (Str s : Space : rest)) | Just n <- readAtx s =
  Header n (tagify rest,[],[]) rest
fixBlock x = x

tagify :: [Inline] -> String
tagify = concatMap toStr
 where
   toStr Space   = "-"
   toStr (Str s) = s
   toStr _       = "Q"  -- arbitrary

readAtx :: String -> Maybe Int
readAtx s | all (== '#') s = Just (length s)
          | otherwise      = Nothing

#ifdef Testing

readDoc :: String -> Pandoc
readDoc = either (error . ("readDoc: readMarkdown failed: " ++) . show) id .
          readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeHtmlString def

rewrite :: String -> String
rewrite = writeDoc
        . bottomUp fixBlock
        . readDoc

#endif
