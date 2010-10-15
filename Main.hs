-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Transfer gitit content to blog
----------------------------------------------------------------------

module Main where

import Data.List (isInfixOf)
import Control.Monad ((>=>))

import Network.HTTP

import Control.Compose ((~>),result,Unop)

main :: IO ()
main = getPost "Adding numbers" >>= putStr

getPost :: String -> IO String
getPost = (result.fmap) trim $ getHtml . postUrl

postsDir :: String
postsDir = "http://localhost:5002/Posts/"

postUrl :: Unop String
postUrl = (postsDir ++) . tidyUrl
-- TODO: parametrize over port #

-- Tidy up a URL.  For now, just turns spaces to "%20"
tidyUrl :: String -> String
tidyUrl = concatMap fixChar
 where
   fixChar ' ' = "%20"
   fixChar c   = [c]

getHtml :: String -> IO String
getHtml = simpleHTTP . getRequest >=> getResponseBody


trim :: String -> String
trim = onLines trimLines

onLines :: Unop [String] -> Unop String
onLines = lines ~> unlines

trimLines :: Unop [String]
trimLines = dropWhile (not . isInfixOf "<!-- references -->")
          . tail  
          . takeWhile (not . isInfixOf "<div id=\"footer\">")

