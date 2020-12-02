#!/usr/bin/env cabal
{- cabal:
        build-depends: base, containers, sbv, monad-memo, criterion
        default-extensions: ParallelListComp, BangPatterns
-}

module Main where

import qualified Data.IntSet as Set
import qualified Data.IntMap as Map
import Data.SBV
import Data.Int
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Monad.Memo
import Criterion.Main

target :: Num n => n
target = 2020

main :: IO ()
main = do
    (vals, set) <- getInput
    defaultMain
        [ bench "smt" $ nfIO $ mainPart2_smt vals set
        , bench "brute" $ nfIO $ mainPart2_brute vals set
        , bench "memoizing" $ nfIO $ mainPart2_DP vals set
        ]

mainPart2_smt :: [Int] -> Set.IntSet -> IO Int64
mainPart2_smt vals set = do
    satRes <- sat $ do
        let xs = literal <$> (map fromIntegral vals :: [Int64])
        a <- symbolic "a"
        b <- symbolic "b"
        c <- symbolic "c"
        constrain $ a `sElem` xs
        constrain $ b `sElem` xs
        constrain $ c `sElem` xs
        constrain $ a + b + c .== literal target
    case extractModel satRes of
        Just (a,b,c) -> pure (a*b*c :: Int64)
        Nothing -> error "foo"

mainPart2_brute :: [Int] -> Set.IntSet -> IO Int
mainPart2_brute vals _ = do
    let sols = subsetsums vals []
    case sols of
        [a,b,c]:_ -> pure (a*b*c)
        _ -> error "nooooo"
  where
    size = 3
    subsetsums xs curr
        | sum curr == target && length curr == size = [curr]
        | otherwise = do
            guard (length curr < size)
            x <- xs
            guard (not (x `elem` curr))
            guard (sum (x:curr) <= target)
            subsetsums xs (x:curr)

mainPart2_DP :: [Int] -> Set.IntSet -> IO Int
mainPart2_DP vals valsSet = do
    case startEvalMemo (subsetsums (3, 2020)) of
        Just [a,b,c] -> pure (a*b*c)
        _ -> error "nooooo"
  where
    subsetsums :: (Int, Int) -> Memo (Int,Int) (Maybe [Int]) (Maybe [Int])
    subsetsums (1, n) = if n `Set.member` valsSet then pure (Just [n]) else pure Nothing
    subsetsums !(!k, !n) = do
        let go [] = pure Nothing
            go (v:vs) = do
                res <- memo subsetsums (k-1, n - v)
                case res of
                    Just r -> pure $ Just (v,r)
                    Nothing -> go vs
        sol <- go vals
        case sol of
            Just (v,xs) -> pure $ Just (v : xs)
            Nothing -> pure Nothing

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
