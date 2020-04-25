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

randbool :: IO Bool 
randbool = randomRIO (False, True)

randomlist x = take x $ randoms (mkStdGen 5) :: [Bool]

--randomgraph i = Graph (map (\x -> map (\y -> randbool) [0..x-1]) [0..i-1])

randomgraph :: Int -> Graph
randomgraph i = Graph (map (\x -> randomlist x) [0..i-1])


--take (x-1) randomlist


-- main = randomgraph 4 >> show >> putStrLn
