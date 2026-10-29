module Regex where

import Data.List (sort)

-- Data invul manier:
--[a-z0-9] -> CharSet False [('a', 'z'), ('0', '9')]
--[^x]     -> CharSet True [('x', 'x')]
-- .       -> CharSet True []
data CharSet = CharSet Bool [(Char, Char)]
  deriving (Eq, Ord, Show)

memberSet :: Char -> CharSet -> Bool
memberSet c (CharSet neg rs) = neg /= any inRange rs
  where
    inRange (lo, hi) = lo <= c && c <= hi

anyChar :: CharSet
anyChar = CharSet True []

singleton :: Char -> CharSet
singleton c = CharSet False [(c, c)]

digitSet, wordSet, spaceSet :: CharSet
digitSet = CharSet False [('0', '9')]
wordSet  = CharSet False [('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_')]
spaceSet = CharSet False [(c, c) | c <- " \t\n\r\f\v"]

-- [abc] ==> [^abc] en andersom. Gebruikt voor \D, \W en \S.
complementSet :: CharSet -> CharSet
complementSet (CharSet neg rs) = CharSet (not neg) rs

-- De bereiken uit een set halen. Gebruikt voor \d binnen [ ].
setRanges :: CharSet -> [(Char, Char)]
setRanges (CharSet _ rs) = rs


data Regex
  = Empty            -- ∅ : matcht niets
  | Eps              -- ε : matcht alleen de lege string
  | Chr CharSet         -- één bepaald teken
  | Seq Regex Regex  -- r s : eerst r, dan s
  | Alt Regex Regex  -- r|s : r of s
  | Star Regex       -- r*  : nul of meer keer r
  deriving (Eq, Ord, Show)

-- Kan r de lege string matchen?
nullable :: Regex -> Bool
nullable Empty     = False
nullable Eps       = True
nullable (Chr _)   = False
nullable (Seq a b) = nullable a && nullable b
nullable (Alt a b) = nullable a || nullable b
nullable (Star _)  = True

-- Concatenatie
cat :: Regex -> Regex -> Regex
cat Empty _     = Empty
cat _ Empty     = Empty
cat Eps r       = r
cat r Eps       = r
cat (Seq a b) c = cat a (cat b c)
cat a b         = Seq a b

-- keuze
alt :: Regex -> Regex -> Regex
alt a b = case dedup (sort (alternatives a ++ alternatives b)) of
  [] -> Empty
  rs -> foldr1 Alt rs
  where
    alternatives Empty     = []
    alternatives (Alt x y) = alternatives x ++ alternatives y
    alternatives r         = [r]

    dedup (x : y : rest)
      | x == y    = dedup (y : rest)
      | otherwise = x : dedup (y : rest)
    dedup xs = xs

-- Ster:  ∅* = ε* = ε   en   r** = r*
star :: Regex -> Regex
star Empty      = Eps
star Eps        = Eps
star r@(Star _) = r
star r          = Star r


-- Simpele unoptimized Deriv
derivNaive :: Char -> Regex -> Regex
derivNaive _ Empty = Empty
derivNaive _ Eps   = Empty
derivNaive c (Chr s)
  | c `memberSet` s = Eps
  | otherwise       = Empty
derivNaive c (Seq a b)
  | nullable a = Alt (Seq (derivNaive c a) b) (derivNaive c b)
  | otherwise  = Seq (derivNaive c a) b
derivNaive c (Alt a b) = Alt (derivNaive c a) (derivNaive c b)
derivNaive c (Star a)  = Seq (derivNaive c a) (Star a)

-- Optimized Deriv
deriv :: Char -> Regex -> Regex
deriv _ Empty = Empty
deriv _ Eps   = Empty
deriv c (Chr s)
  | c `memberSet` s = Eps
  | otherwise       = Empty
deriv c (Seq a b)
  | nullable a = alt (cat (deriv c a) b) (deriv c b)
  | otherwise  = cat (deriv c a) b
deriv c (Alt a b)  = alt (deriv c a) (deriv c b)
deriv c r@(Star a) = cat (deriv c a) r

-- Hoeveel knopen heeft de boom? (om het verschil te meten)
size :: Regex -> Int
size (Seq a b) = 1 + size a + size b
size (Alt a b) = 1 + size a + size b
size (Star a)  = 1 + size a
size _         = 1

-- Leid af naar elk teken en kijk dan of de lege string overblijft.
matches :: Regex -> String -> Bool
matches r s = nullable (foldl (flip derivNaive) r s)

-- matches (Seq (Chr 'a') (Star (Chr 'b'))) "ab"
-- abc
-- (Seq (Star (Alt (Chr 'a') (Chr 'b'))) (Star (Chr 'c')))

-- helper functions for testing

-- One literal character, and a whole literal string
lit :: Char -> Regex
lit c = Chr (singleton c)

str :: String -> Regex
str = foldr (cat . lit) Eps

-- r+ = r r*
plus :: Regex -> Regex
plus r = cat r (star r)

-- r? = ε | r
opt :: Regex -> Regex
opt r
  | nullable r = r
  | otherwise = alt Eps r

-- r{n}, r{n,} en r{n,m}.  Nothing als bovengrens = geen bovengrens.
repeatRange :: Int -> Maybe Int -> Regex -> Regex
repeatRange n upper r = cat (times n) rest
  where
    times k = foldr cat Eps (replicate k r)
    rest = case upper of
      Nothing -> star r
      Just m  -> optionals (m - n)
    -- r{0,k} = (r(r(r)?)?)?  — genest, zodat er geen dubbele paden ontstaan
    optionals k
      | k <= 0    = Eps
      | otherwise = opt (cat r (optionals (k - 1)))