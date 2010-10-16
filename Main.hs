-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}  -- TEMP
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

import Data.List (isPrefixOf,isSuffixOf,isInfixOf,group)
import Data.Char (isAlpha,isDigit,toLower)
import Control.Applicative ((<$>))
-- import Control.Monad ((>=>))

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.UTF8 as U
import Network.Curl.Download

import Network.XmlRpc.Client (Remote,remote)
-- import Network.XmlRpc.Internals (Value(..))

import Control.Compose ((~>),result,Unop)

-- main :: IO Value
-- main = blogRpc "wp.getAuthors"
-- main = getBlogPost 100 >>= putStr

main :: IO ()
main = getWikiPost "Adding numbers" >>= writeFile "post.html"

-- main :: IO ()
-- main = getHtml (postUrl "Adding numbers") >>= putStrLn

{--------------------------------------------------------------------
    Gather content from gitit
--------------------------------------------------------------------}

getWikiPost :: String -> IO String
getWikiPost = (result.fmap) adjustWikiContent $ getHtml . postUrl

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
getHtml = (fmap.fmap) (either id U.toString) openURI

-- TODO: Handle errors (Left) in openURI

-- getHtml = simpleHTTP . getRequest >=> getResponseBody

adjustWikiContent :: Unop String
-- adjustWikiContent = id
adjustWikiContent = onLines (map tweakLine . trimLines)

onLines :: Unop [String] -> Unop String
onLines = lines ~> unlines

{--------------------------------------------------------------------
    Change header levels
--------------------------------------------------------------------}

-- I want h1, h2, ... for my wiki, but h3, h4, ... for my blog.
-- Adjust here.  The Pandoc-generated html lines look like "><h2".
tweakLine :: Unop String

tweakLine line
  | isPrefixOf "><h" line && (line!!3) `elem` ['1'..'7']
    = onNth 3 (succ.succ) line

tweakLine line@(_:_)
  | isSuffixOf "></h" line' && end `elem` ['1'..'7']
    = line' ++ [succ . succ $ end]
 where
   line' = init line
   end = last line

tweakLine line = line

onNth :: Int -> Unop a -> Unop [a]
onNth 0 f (x:xs) = f x : xs
onNth n f (x:xs) = x : onNth (n-1) f xs
onNth _ _ xs = xs


-- tweakLine ('>':'<':'h':c:rest)
--   | c `elem` ['1'..'7'] = ('>':'<':'h':succ (succ c):rest)


{--------------------------------------------------------------------
    Trim extra gitit html
--------------------------------------------------------------------}

trimLines :: Unop [String]
trimLines = dropWhile (not . isInfixOf "<!-- references -->")
          . tail  
          . takeWhile (not . isInfixOf "<div id=\"footer\">")


{--------------------------------------------------------------------
    Send to my blog
--------------------------------------------------------------------}

getBlogPost :: String -> IO String
getBlogPost name =
  getPassword >>= blogRpc "metaWeblog.getPost" (postId name) user

password :: String
password = unsafePerformIO getPassword

blogRpc :: Remote a => String -> a
blogRpc cmd = remote blogRpcUrl cmd blogId user password

blogId :: Int
blogId = 1  -- default

blogRpcUrl :: String
blogRpcUrl = "http://conal.net/blog/xmlrpc.php"

getPassword :: IO String
getPassword = head . lines <$> readFile "/Users/conal/.ssh/blog-password"

user :: String
user = "conal"

{--------------------------------------------------------------------
    Construct post Id from title
--------------------------------------------------------------------}

-- Tweak a name into a post Id
-- Example: @postId "Is \"reality\" over-rated?" == "is-reality-over-rated"@
postId :: Unop String
postId = tidyHyphens . map (hyphenate . toLower)
 where
   -- Replace sequences of hyphens by a single hyphen
   tidyHyphens :: Unop String
   tidyHyphens = trimHead . trimTail . concat . map collapse  . group
    where
      collapse cs | all (== '-') cs = "-"
                  | otherwise       = cs
      trimHead = dropWhile (== '-')
      trimTail = inReverse trimHead

   okNameChar :: Char -> Bool
   okNameChar c = isAlpha c || isDigit c

   hyphenate :: Unop Char
   hyphenate c | okNameChar c = c
               | otherwise    = '-'

inReverse :: Unop ([a] -> [b])
inReverse = reverse ~> reverse
