{-# LANGUAGE CPP, PatternGuards, ScopedTypeVariables, ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Network.Gitit.Plugin.FixSymbols
-- Copyright   :  (c) Conal Elliott 2010-2013
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Turn some Haskell symbols into pretty math symbols
-- Many are selected from <http://xahlee.org/Periodic_dosage_dir/unicode.html>
-- 
-- You can also add specialized rewrites via the "subst-map" metadata
-- field, e.g.,
-- 
--   substMap: [("abutWE","⇔"), ("abutSN","⇕")]
----------------------------------------------------------------------

module Network.Gitit.Plugin.FixSymbols
  ( plugin, rewriter, fixInline, fixBlock, Unop, Subst, lookupSubst
  ) where

-- #define NumSuffix

import Network.Gitit.Interface

import Data.Char (isUpper,isSpace)
import Data.Maybe (fromMaybe,isJust,listToMaybe)
import Data.List (isSuffixOf,intercalate)
import Data.Monoid (Monoid(..),(<>))
import Control.Monad ((<=<))
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative ((<$>))
#ifdef NumSuffix
import Control.Arrow ((***))
import Data.Tuple (swap)
import Data.Char (isDigit)
#endif

import Control.Monad.State.Class (get)

-- Experimental
import Data.Default
import Text.Pandoc.Options (ReaderOptions(..))
import Text.Pandoc.Readers.Markdown
import Data.String
import Data.Text (Text)
import Data.Yaml

-- | Transformation
type Unop a = a -> a

rewriter :: Subst -> Unop Pandoc
rewriter specials = bottomUp (fixInline specials) . bottomUp (fixBlock specials)

plugin :: Plugin
-- plugin = PageTransform $ return . rewriter

plugin = PageTransform $ \ p ->
  do Context { ctxMeta = meta } <- get
     liftIO $ putStrLn $ "meta data: " ++ show meta
     let specials = lookupSubst (Map.fromList meta)
     -- liftIO $ putStrLn $ "specials: " ++ show specials
     return $ rewriter specials p

lookupSubst :: Map String String -> Map String String
lookupSubst meta =
  Map.fromList (fromMaybe [] (read <$> Map.lookup "substMap" meta))
  <> fromMaybe mempty ((mvToSubst <=< parseSubstMV <=< Map.lookup "subst") meta)

-- In Text.Pandoc.Readers.Markdown:
-- 
--   toMetaValue :: ReaderOptions -> Text -> Either PandocError MetaValue
--
-- But not exported. :(
-- Instead, use readMarkdown.

str1 :: String
str1 = "[\"&&& △\",\"*** ×\",\"||| ▽\",\"+++ +\",\"|- ⊢\",\"<~ ⤺\",\"k (↝)\",\"op (⊙)\",\"--> ⇨\",\"+-> ➔\",\":*: ✖\",\":+: ➕\",\":->? ⤔\",\"Unit ()\",\"R ℝ\",\"Unit 𝟙\"]"

parseSubstMV :: String -> Maybe MetaValue
parseSubstMV str =
  case readMarkdown def ("---\nsubst: " ++ str ++ "\n---") of
    Right (Pandoc (Meta m) _) -> Map.lookup "subst" m
    Left _                    -> Nothing

-- [MetaList [MetaInlines [Str "Unit",Space,Str "()"],...]]
mvToSubst :: MetaValue -> Maybe Subst
mvToSubst (MetaList m) = Map.fromList <$> mapM toStr m
 where
   toStr (MetaInlines [Str from,Space,Str to]) = Just (from,to)
   toStr _                       = Nothing
mvToSubst _ = Nothing


-- Oops: "+" becomes `MetaBlocks [BulletList [[]]]`
-- instead of `MetaInlines [Str "+"]`

-- Maybe I don't really want to use JSON style maps for subst, since I don't
-- want gitit to parse them.

mkSubst :: MetaValue -> Subst
mkSubst (MetaMap m) = unString <$> m
 where
   unString (MetaString str) = str
   unString v = "oops: " ++ show v
mkSubst _ = mempty

-- mkPageTransform :: Data a => (a -> a) -> Plugin
-- mkPageTransform fn = PageTransform $ return . bottomUp fn

defaultHaskellInline :: Bool
defaultHaskellInline = True -- False

fixInline :: Subst -> Unop Inline
fixInline specials (Code attr@(ident,classes,keyVals) s)
  | "url" `notElem` classes = Code attr' (translate subst s)
 where
    subst = specials `mappend` substMap
    -- default to haskell if no classes are given
    attr' | null classes && defaultHaskellInline = (ident,["haskell"],keyVals)
          | otherwise                            = attr
fixInline _ x               = x

-- The url exception is thanks to Travis Cardwell.

fixBlock :: Subst -> Unop Block
fixBlock specials (CodeBlock attr@(_,classes,_) s)
  | "haskell" `elem` classes = CodeBlock attr (translate subst (dropBogus s))
 where
    subst = specials `mappend` substMap
fixBlock _ x = x

-- Drop lines that end with: error "bogus case pending compiler fix"
-- To do: maybe move to another plugin
dropBogus :: Unop String
dropBogus = unlines . filter (not . bogus) . lines

bogus :: String -> Bool
bogus = isSuffixOf "error \"bogus case pending compiler fix\""

-- | String substitution
type Subst = Map String String

translate :: Subst -> Unop String
translate subst = concat . fixInfix . map (translateLex subst) . fixLex . lexString

-- Turn "`(+)`" into "+", but before concat'ing
fixInfix :: Unop [String]
fixInfix [] = []
fixInfix ("`":s:"`":ss) | Just op <- stripParens s = op : fixInfix ss
fixInfix (s : ss) = s : fixInfix ss

isQuantifier :: String -> Bool
isQuantifier = (`elem` ["forall","exists","∀","∃","\\/"])

-- Misc tweaks on lexeme streams, including determining whether a "." is a
-- function composition, part of forall, a qualified name, or the end of a
-- sentence in a comment.
fixLex :: Unop [String]
fixLex [] = []
fixLex ("[":"|":ss) = "[|" : fixLex ss   -- open  semantic bracket
fixLex ("|":"]":ss) = "|]" : fixLex ss   -- close semantic bracket
fixLex ("[":"=":ss) = "[=" : fixLex ss   -- less defined ⊑
fixLex ("]":"=":ss) = "]=" : fixLex ss   -- more defined ⊒
fixLex (ss@(q:_)) | isQuantifier q       -- forall a b c. ...
                  , (before,(".":after)) <- break (== ".") ss =
 before ++ (dotLex : fixLex after)   
fixLex (s@(c:_):".":ss) | isUpper c = fixLex ((s++"."):ss) -- qualified name
fixLex (s@(c:_):('.':s'):ss) | isUpper c = fixLex ((s++"."):s':ss) -- qualified name
fixLex (s : ss) = s : fixLex ss

dotLex :: String
dotLex = "#dot#"

stripParens :: String -> Maybe String
stripParens ('(':s) | not (null s) && last s == ')' = Just (init s)
stripParens _ = Nothing

translateLex :: Subst -> Unop String
translateLex subst ('\\':s') | isJust (Map.lookup s' subst) = s'
translateLex subst s = numSuffix $ fromMaybe s $ Map.lookup s subst

-- The first translateLex case avoids rewriting backslash-escaped lexemes.

numSuffix :: Unop String
#if 1
-- highlighting-kate 0.6.2.1 splits non-ascii characters.
-- See <https://github.com/jgm/highlighting-kate/issues/92>
numSuffix = id
#else
numSuffix str | null pre  = str
              | otherwise = pre ++ map tweak suf
 where
   (pre,suf) = spanr isDigit str
   tweak c = toEnum (fromEnum c + offset)
   offset = fromEnum '₀' - fromEnum '0'

-- Like span but from the right
spanr :: (a -> Bool) -> [a] -> ([a],[a])
spanr p = swap . (reverse *** reverse) . span p . reverse
#endif

{- 

I tried thin spaces (http://www.cs.tut.fi/~jkorpela/chars/spaces.html)
to make up for substituted symbols often being shorter.

three-per-em space 	foo bar
four-per-em space 	foo bar
six-per-em space 	foo bar
hair space	 	foo bar

However, standard fat spaces get substituted for these thin ones.

-}

substMap :: Subst
substMap = Map.fromList $
  [ ("<=","≤"), (">=", "≥")
  , ("intersect", "(∩)"), ("union", "(∪)"), ("elem", "(∈)"), ("member", "(∈)")
  , ("forall","∀"),("exists","∃"), ("\\/","∀"),(dotLex,".")
  , ("undecided", "…")
  , ("->","→"),(".","∘"),(":*","×"),(":+","+"),("=>","⇒"), ("<==>","⟺") -- or "⇔"
  , (":*:","×"), (":+:","+"), (":.:","∘"), (":^:","⇑"), (":>:","↦")
  -- , (":.","∘")
  , (":-*","⊸")
  , ("\\","λ"), ("/\\","Λ")
  , ("lub","(⊔)"), ("glb","(⊓)"), ("[=","⊑"), ("]=","⊒")
  , ("mempty","∅"), ("mappend","(♢)"),("<>","♢")
  -- , ("mappend","(⊕)") -- , ("op","(⊙)")
  -- , ("<*>","⊛")
  , ("undefined","⊥") -- , ("bottom","⊥")
  , ("<-","←"), ("-<", "⤙") -- "−≺"
  , ("::","∷"), ("..","‥"), ("...","⋯")
  , ("==","≡"), ("/=","≠")
  , ("=~","≅")
  -- Move subsets of the rest into specific pages, via the "subst-map"
  -- metadata tag.
  , (":->", "↣"), (":->:","↣") -- or ⇢, ↦, ⇰, ➵, ➟
  , (":>", "⇴")
  , (":-+>", "☞"), ("-->", "⇢"), ("~>", "↝"), ("~>*", "↝*"), ("<~", "↜"), ("*<~", "*↜"), (":=>","⇨") 
  , ("+>", "↦"), ("<+", "↤")
  , (":^+", "➴"), (":+^", "➶") -- top-down vs bottom-up comp -- ↥ ↧ ↱↰ ↥ ↧ ⇤ ⇥ ⤒ ↱ ↲ ↳ ↰ ➷ ➸ ➹
  -- Sadly, I'm not seeing the semantic bracket characters in Firefox 21,
  -- though I see them in Chrome and Safari. Is my problem with the default font?
  , ("[|","⟦"), ("|]","⟧")  -- semantic brackets
  -- , ("[|","["), ("|]","]")
  , ("||","∨"), ("&&","∧") -- maybe
  , ("abutWE","(⇔)"), ("abutSN","(⇕)")
  -- Experimental. Notation from "Calculating Functional Programs"
  -- Moved into specific blog posts
  -- , ("&&&","△"), ("***", "×"), ("|||","▽"), ("+++","+")
  , ("alpha","α") , ("beta","β") , ("gamma","γ") , ("delta","δ")
  , ("epsilon","ε") , ("zeta","ζ") , ("eta","η") , ("theta","θ")
  , ("iota","ι") , ("kappa","κ") , ("lambda","λ") , ("mu","μ") , ("nu","ν")
  , ("xi","ξ") , ("omicron","ο") , ("pi","π") , ("rho","ρ") , ("sigma","σ")
  , ("tau","τ") , ("upsilon","υ") , ("phi","φ") , ("chi","χ") , ("psi","ψ")
  , ("omega","ω")

  , ("Alpha","Α") , ("Beta","Β") , ("Gamma","Γ") , ("Delta","Δ")
  , ("Epsilon","Ε") , ("Zeta","Ζ") , ("Eta","Η") , ("Theta","Θ")
  , ("Iota","Ι") , ("Kappa","Κ") , ("Lambda","Λ") , ("Mu","Μ") , ("Nu","Ν")
  , ("Xi","Ξ") , ("Omicron","Ο") , ("Pi","Π") , ("Rho","Ρ") , ("Sigma","Σ")
  , ("Tau","Τ") , ("Upsilon","Υ") , ("Phi","Φ") , ("Chi","Χ") , ("Psi","Ψ")
  , ("Omega","Ω")

  ]

-- The 'reverse' is to apply earlier rewrites first.  Or flip (.)

-- Experiments in string dissection. Simplified lexing.

-- | Dissect a string of haskell code into lexemes. Mainly uses
-- Prelude's 'lex', but preserves spaces and end-of-line comments.
lexString :: String -> [String]
lexString "" = []
lexString (s@('-':'-':' ':_)) = line : lexString rest
 where
   (line,rest) = break (== '\n') s
lexString (c:s') | c `elem` " \n\t" = [c] : lexString s'
lexString s | [(h,t)] <- lex s = h : lexString t
lexString (c:s') = [c] : lexString s'

-- To do: Fix up code segments within comments.

{--------------------------------------------------------------------
    Fix old-style substMap
--------------------------------------------------------------------}



updateSubst :: Unop String
updateSubst s =
  case updateSubstS s of
    [(s',[])] -> s'
    _         -> s

updateSubstS :: ReadS String
updateSubstS s0 =
  do ("substMap",':': (dropWhile isSpace -> s1)) <- lex s0
     (pairs :: [(String,String)], s2) <- reads s1
     return ("subst: [" ++ intercalate "," (map updPair pairs) ++ "]", s2)
 where
   updPair (from,to) = "\"" ++ from ++ " " ++ to ++ "\""
