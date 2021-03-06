#!/usr/bin/env cabal
{- cabal:
    build-depends: base, containers
-}

import Data.Maybe

data Policy = Policy
    { letter :: Char
    , low :: Int
    , high :: Int
    }
data Entry = Entry
    { policy :: Policy
    , password :: String
    }

readEntries :: FilePath -> IO [Entry]
readEntries fp = map readEntry . lines <$> readFile fp
  where
    readEntry str =
        let (l,r1):_ = reads str
            (h,r2):_ = reads (drop 1 r1)
            (c:_,r3) = break (== ':') (drop 1 r2)
            p = drop 2 r3
        in Entry (Policy c l h) p

isValid :: Entry -> Bool
isValid (Entry pol pass) =
    let len = length (filter (== letter pol) pass)
    in len <= high pol && len >= low pol

isValid_part2 :: Entry -> Bool
isValid_part2 (Entry pol pass) =
    let indexpass = zip [1..] pass
        chars = catMaybes [lookup (low pol) indexpass, lookup (high pol) indexpass]
        len = length (filter ((== letter pol)) chars)
    in len == 1

main = do
    entries <- readEntries "input"
    print $ length (filter isValid_part2 entries)
