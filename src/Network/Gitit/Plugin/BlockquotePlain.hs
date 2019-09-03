{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- TEMP

-- | When there's no blank line after the last paragraph within a <blockquote>
-- tag, we get a Plain block. Change it to a Para. Look for a Plain block
-- followed by 'RawBlock (Format "html") "</blockquote>"'

module Network.Gitit.Plugin.BlockquotePlain (plugin, fixBlocks) where

import Network.Gitit.Interface

-- | Transformation
type Unop a = a -> a

plugin :: Plugin
plugin = PageTransform $ return . bottomUp fixBlocks

-- Non-recursive version, driven by bottomUp
fixBlocks :: Unop [Block]
fixBlocks (bq@(RawBlock (Format "html") "<blockquote>") : Plain inlines : rest) =
  (bq : Para inlines : rest)
fixBlocks (Plain inlines : bq@(RawBlock (Format "html") "</blockquote>") : rest) =
  (Para inlines : bq : rest)
fixBlocks blocks = blocks
