module Graph where
import System.Random

data Graph = Graph [[Bool]]
    deriving Show

edge :: Graph -> Int -> Int -> Bool
edge (Graph l) x y | x == y = False
edge (Graph l) x y = (l !! (max x y)) !! (min x y)

len :: Graph -> Int
len (Graph l) = length l

empty :: Int -> Graph
empty i = Graph (map (\x -> map (\y -> False) [0..x-1]) [0..i-1])

clique :: Int -> Graph
clique i = Graph (map (\x -> map (\y -> True) [0..x-1]) [0..i-1])

purerandomlist :: StdGen -> Int -> [Bool]
purerandomlist g x = take x $ randoms g

randomgraph :: IO StdGen -> Int -> IO Graph
randomgraph g i = fmap (\z -> Graph (map (\x -> purerandomlist z x) [0..i-1])) g

main = do
    ((fmap show (randomgraph newStdGen 5)) >>= putStrLn)
