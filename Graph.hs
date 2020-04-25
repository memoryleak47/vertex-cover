module Graph where

data Graph = Graph [[Bool]]
    deriving Show

edge :: Graph -> Int -> Int -> Bool
edge (Graph l) x y | x == y = False
edge (Graph l) x y = (l !! (max x y)) !! (min x y)

empty :: Int -> Graph
empty i = Graph (map (\x -> map (\y -> False) [0..x-1]) [0..i-1])

clique :: Int -> Graph
clique i = Graph (map (\x -> map (\y -> True) [0..x-1]) [0..i-1])

len :: Graph -> Int
len (Graph l) = length l

main = putStrLn $ show $ edge (clique 3) 0 1
