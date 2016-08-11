{-# LANGUAGE PatternGuards, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Network.Gitit.Plugin.Comment
-- Copyright   :  (c) Conal Elliott 2011
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Gitit plugin: remove comments like <!--[ ... ]-->
----------------------------------------------------------------------

module Network.Gitit.Plugin.Comment (plugin,fixInline,fixBlock) where

import Data.List (isPrefixOf,tails,findIndex)
import Control.Arrow (second)

import Network.Gitit.Interface

plugin :: Plugin

-- plugin = PageTransform $ return . bottomUp (concatMap fixBlock) . bottomUp (concatMap fixInline)

plugin = PageTransform $ return . bottomUp fixBlock . bottomUp fixInline

-- plugin = mkPageTransform fixInline

-- fixInline :: Inline -> [Inline]
-- fixInline (RawInline "html" s) | isPrefixOf "<!--[" s && isSuffixOf "]-->" s = []
-- fixInline x = [x]

-- | Unary transformation
type Unop a = a -> a

fixInline :: Unop Inline
fixInline (RawInline "html" s) = RawInline "html" (removeComments s)
fixInline x = x

removeComments :: Unop String
removeComments = removeBracketed "<!--[" "]-->"

removeBracketed :: Eq a => [a] -> [a] -> Unop [a]
removeBracketed open close = beforeOpen
 where
   beforeOpen  as | Just (pre,post) <- splitInfix open  as = pre ++ beforeClose post
                  | otherwise = as
   beforeClose as | Just (_  ,post) <- splitInfix close as = beforeOpen post
                  | otherwise = as  -- no close. hm.

splitInfix :: Eq a => [a] -> [a] -> Maybe ([a],[a])
splitInfix p s = fmap f (findInfix p s)
 where
   f i = second (drop (length p)) (splitAt i s)

findInfix :: Eq a => [a] -> [a] -> Maybe Int
findInfix p = findIndex (isPrefixOf p) . tails

-- Example:
-- 
--   *> removeBracketed "(" ")" "as(df)j(kl)m"
--   "asjm"
--
-- But:
-- 
--   *Network.Gitit.Plugin.Comment> removeBracketed "(" ")" "as(b(df)j(kl)q)m"
--   "asjq)m"

-- TODO: Handle *nested* brackets
-- TODO: More elegant definition. I don't like state machines.

-- After emptying some html inlines, we might have empty blocks.
-- I haven't tested this plugin enough to see what happens.

-- fixBlock :: Block -> [Block]
-- fixBlock (Plain []) = []
-- fixBlock x = [x]

fixBlock :: Unop Block
fixBlock (RawBlock "html" s) = RawBlock "html" (removeComments s)
fixBlock x = x
