#!/usr/bin/env cabal
{- cabal:
        build-depends: base, containers, sbv
-}

module Main where

import qualified Data.IntSet as Set
import Data.SBV
import Data.Int

target :: Int
target = 2020

main :: IO ()
main = mainPart2

mainPart2 :: IO ()
mainPart2 = do
    (vals,set) <- getInput
    satRes <- sat $ do
        let xs = literal <$> (map fromIntegral vals :: [Int64])
        a <- symbolic "a"
        b <- symbolic "b"
        c <- symbolic "c"
        constrain $ a `sElem` xs
        constrain $ b `sElem` xs
        constrain $ c `sElem` xs
        constrain $ a + b + c .== literal (fromIntegral target)
    case extractModel satRes of
        Just (a,b,c) -> do
            print a
            print b
            print c
            print (a*b*c :: Int64)
        Nothing -> error "foo"

mainPart1 :: IO ()
mainPart1 = do
    (vals,set) <- getInput
    let matches = filter (\v -> (2020 - v) `Set.member` set) vals
    case matches of
        (x:_) -> print (x * (2020 - x))
        _ -> error "No matching pair"

getInput :: IO ([Int], Set.IntSet)
getInput = do
    vals <- map read . words <$> readFile "input.txt"
    let set = Set.fromList vals
    pure (vals,set)
