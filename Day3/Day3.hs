#!/usr/bin/env cabal
{- cabal:
    build-depends: base, containers
-}

type Input = [String]

getInput :: IO Input
getInput =
    map cycle . lines <$> readFile "input"

main :: IO ()
main = do
    ipt <- getInput
    let treeCount13 = move 0 1 0 3 ipt 0
    let treeCount11 = move 0 1 0 1 ipt 0
    let treeCount15 = move 0 1 0 5 ipt 0
    let treeCount17 = move 0 1 0 7 ipt 0
    let treeCount21 = move 0 2 0 1 ipt 0
    print $ foldr1 (*) [treeCount13, treeCount11, treeCount15, treeCount17, treeCount21]

move :: Int -> Int -> Int -> Int -> Input -> Integer -> Integer
move up down left right [] count = count
move up down left right ipt count
    | up > 0 || left > 0 = error "up? left?"
    | otherwise =
        let movedDown = drop down ipt
            movedRightAndDown = map (drop right) movedDown
            isTree = map (take 1) (take 1 movedRightAndDown) == ["#"]
            newCount = count + fromIntegral (fromEnum isTree)
        in move up down left right movedRightAndDown newCount
