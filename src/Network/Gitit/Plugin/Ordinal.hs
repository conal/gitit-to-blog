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
fixInline (Str s) | Just (n,suff,punct) <- splitOrdinal s =
  [Str (show n), Superscript [Str suff], Str punct]
fixInline x = [x]

-- | Split an ordinal string in prefix & suffix. For instance, "21st" ->
-- Just ("21","st"). Note: allows questionable ordinals like "23th".

splitOrdinal :: String -> Maybe (Int,String,String)
-- splitOrdinal s | [(n,suff)] <- reads s, isOrdinalSuffix suff = Just (n,suff,"")
-- splitOrdinal _ = Nothing
splitOrdinal s = do (n,suff1) <- listToMaybe (reads s)
                    let (osuff, punct) = splitAt 2 suff1
                    guard (isOrdinalSuffix osuff && all isPunctuation punct)
                    return (n,osuff,punct)

isOrdinalSuffix :: String -> Bool
isOrdinalSuffix = (`elem` ["st", "nd", "rd", "th"])

