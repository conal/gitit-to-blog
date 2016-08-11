-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  BirdtrackShift
-- Copyright   :  (c) Conal Elliott 2011
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Indent "<foo" so it won't be treated as an inverse birdtrack literate
-- Haskell program.
----------------------------------------------------------------------

module Network.Gitit.Plugin.BirdtrackShift (plugin,process) where

import Data.Char (isSpace)
import Network.Gitit.Interface

-- | Unary transformation
type Unop a = a -> a

plugin :: Plugin
plugin = PreParseTransform $ return . process

-- TODO: indent only if we're sensitive to bird tracks. Something like
-- 
--   plugin = PreParseTransform $ getContext >>= process . ctxBirdTracks
--   process :: Bool -> Unop String

process :: Unop String
process = (onLines . map) indentTag  -- (I like semantic editor combinators.)

onLines :: Unop [String] -> Unop String
onLines = (unlines .) . (. lines)

-- Lines that start with HTML tags are interpreted as Haskell code, so
-- indent by one space. For instance, "<div>" --> " <div>".
indentTag :: Unop String
indentTag s@('<':c:_) | not (isSpace c) = ' ' : s
indentTag s                             = s
