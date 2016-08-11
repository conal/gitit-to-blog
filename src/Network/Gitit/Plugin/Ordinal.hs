{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Network.Gitit.Plugin.FixSymbols
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Gitit plugin: Format ordinals like 21st
----------------------------------------------------------------------

module Network.Gitit.Plugin.Ordinal (plugin, fixInline) where

import Network.Gitit.Interface

plugin :: Plugin
plugin = PageTransform $ return . bottomUp (concatMap fixInline)

fixInline :: Inline -> [Inline]
fixInline (Str s) | Just (n,suff) <- splitOrdinal s =
  [Str (show n), Superscript [Str suff]]
fixInline x = [x]

-- | Split an ordinal string in prefix & suffix. For instance, "21st" ->
-- Just ("21","st"). Note: allows questionable ordinals like "23th".

splitOrdinal :: String -> Maybe (Int,String)
splitOrdinal s | [(n,suff)] <- reads s, isOrdinalSuffix suff = Just (n,suff)
splitOrdinal _ = Nothing

isOrdinalSuffix :: String -> Bool
isOrdinalSuffix = (`elem` ["st", "nd", "rd", "th"])

