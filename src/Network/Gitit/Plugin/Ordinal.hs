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

import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import Data.Char (isPunctuation)

import Network.Gitit.Interface

plugin :: Plugin
plugin = PageTransform $ return . bottomUp (concatMap fixInline)

fixInline :: Inline -> [Inline]
fixInline (Str s) | Just (punctA,n,suff,punctB) <- splitOrdinal s =
  [Str punctA, Str (show n), Superscript [Str suff], Str punctB]
fixInline x = [x]

-- | Split an ordinal string in prefix & suffix. For instance, "21st" ->
-- Just ("21","st"). Note: allows questionable ordinals like "23th".

splitOrdinal :: String -> Maybe (String,Int,String,String)
-- splitOrdinal s | [(n,suff)] <- reads s, isOrdinalSuffix suff = Just (n,suff,"")
-- splitOrdinal _ = Nothing
splitOrdinal s = do let (punctA,suff0) = span isPunctuation s
                    (n,suff1) <- listToMaybe (reads suff0)
                    let (osuff, punctB) = splitAt 2 suff1
                    guard (isOrdinalSuffix osuff && all isPunctuation punctB)
                    return (punctA,n,osuff,punctB)

isOrdinalSuffix :: String -> Bool
isOrdinalSuffix = (`elem` ["st", "nd", "rd", "th"])

