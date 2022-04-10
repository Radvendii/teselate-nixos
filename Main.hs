#!/usr/bin/env runghc

module Main where

import Data.Maybe (fromMaybe)
import System.Process (readProcess)
import Data.Matrix (Matrix(..), safeGet, fromLists)

logo :: Matrix Char
logo = fromLists
  [ "          ▗▄▄▄       ▗▄▄▄▄    ▄▄▄▖         "
  , "          ▜███▙       ▜███▙  ▟███▛         "
  , "           ▜███▙       ▜███▙▟███▛          "
  , "            ▜███▙       ▜██████▛           "
  , "     ▟█████████████████▙ ▜████▛     ▟▙     "
  , "    ▟███████████████████▙ ▜███▙    ▟██▙    "
  , "           ▄▄▄▄▖           ▜███▙  ▟███▛    "
  , "          ▟███▛             ▜██▛ ▟███▛     "
  , "         ▟███▛               ▜▛ ▟███▛      "
  , "▟███████████▛                  ▟██████████▙"
  , "▜██████████▛                  ▟███████████▛"
  , "      ▟███▛ ▟▙               ▟███▛         "
  , "     ▟███▛ ▟██▙             ▟███▛          "
  , "    ▟███▛  ▜███▙           ▝▀▀▀▀           "
  , "    ▜██▛    ▜███▙ ▜██████████████████▛     "
  , "     ▜▛     ▟████▙ ▜████████████████▛      "
  , "           ▟██████▙       ▜███▙            "
  , "          ▟███▛▜███▙       ▜███▙           "
  , "         ▟███▛  ▜███▙       ▜███▙          "
  , "         ▝▀▀▀    ▀▀▀▀▘       ▀▀▀▘          "
  ]

-- from lens
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}
infixl 1 ??

for = flip map

vecLen :: (Int, Int) -> Int
vecLen (x, y) = x^2 + y^2


width = 43
height = 20

type Transform = (Int, Int)

trans :: Transform -> (Int, Int) -> (Int, Int)
trans (t1, t2) (s1, s2) = (s1 + t1, s2 + t2)

neg :: Transform -> Transform
neg (t1, t2) = (-t1, -t2)

t1 :: Transform
t1 = (40, 5)

t2 :: Transform
t2 = (9, 20)

t0 :: Transform
t0 = (0, 0)

-- combine the two transforms
tNM :: Int -> Int -> Transform
tNM n m = (fst t1 * n + fst t2 * m, snd t1 * n + snd t2 * m)

getCharLocal :: (Int, Int) -> Char
getCharLocal (x, y) = fromMaybe ' ' $ safeGet y x logo

-- silly monoid instance for our purposes.
-- should really be a newtype or something
-- but I can't be bothered
instance Semigroup Char
  where
    a <> b
      | a == ' ' = b
      | b == ' ' = a
      | a == b   = a
      | True = error ("Conflicting characters" <> [a] <> " and " <> [b])
instance Monoid Char
  where
    mempty = ' '

getCharGlobal :: (Int, Int) -> Char
getCharGlobal loc = mconcat . map getCharLocal $ neighborLocs
  where
    -- translate into the lattice's coordinates
    (x1, y1) = t1
    (x2, y2) = t2
    (x, y) = loc
    a = (y*x2 - x*y2) `div` (x2*y1 - x1*y2)
    b = (y*x1 - x*y1) `div` (x1*y2 - x2*y1)
    -- move to a canonical square of the lattice
    root = trans (neg (tNM a b)) loc
    -- look at the images that might touch that square
    dirs :: [Transform]
    dirs = [t0, neg t1, t2, neg t2]
    neighborLocs :: [(Int, Int)]
    neighborLocs = trans <$> dirs ?? root

main :: IO ()
main = do
  cols <- read <$> readProcess "tput" [ "cols" ] []
  rows <- read <$> readProcess "tput" [ "lines" ] []
  let c = cols - 1
  let r = rows - 2 -- TODO: get actual number of lines in the prompt, don't assume it's 1

  -- turn the matrix back into a string, but tiled
  mapM_ putStrLn $
    for [0..r] (\j ->
      for [0..c] (\i ->
        getCharGlobal (i,j)))
