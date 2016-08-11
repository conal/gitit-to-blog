{-# OPTIONS_GHC -Wall #-}

-- | Replace Para with Plain in lists

module Network.Gitit.Plugin.ListNoPara (plugin, fixBlock) where

import Network.Gitit.Interface

-- | Transformation
type Unop a = a -> a

plugin :: Plugin
plugin = PageTransform $ return . bottomUp fixBlock

fixBlock :: Unop Block
fixBlock (BulletList        blocks) = BulletList        (fixes blocks)
fixBlock (OrderedList attrs blocks) = OrderedList attrs (fixes blocks)
fixBlock block                      = block

fixes :: Unop [[Block]]
fixes = (map.map) unPara

unPara :: Unop Block
unPara (Para inlines) = Plain inlines
unPara block          = block
