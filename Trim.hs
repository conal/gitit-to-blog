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
-- Trim the HTML generated by gitit to suit my WordPress blog
----------------------------------------------------------------------

module Main (main) where

import Control.Applicative ((<$>))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

import Control.Compose ((~>),Unop)

main :: IO ()
main = do [path] <- getArgs
          onLines process <$> T.readFile path >>= T.putStr

onLines :: Unop [Text] -> Unop Text
onLines = T.lines ~> T.unlines

process :: Unop [Text]
process = dropWhile (not . has "<!-- references -->")
        . tail  
        . takeWhile (not . has "<div id=\"footer\">")
 where
   has :: String -> Text -> Bool
   has = T.isInfixOf . T.pack

