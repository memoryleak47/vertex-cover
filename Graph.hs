module Graph where
import System.Random

data Graph = Graph [[Bool]]
    deriving Show

iter :: Int -> [(Int,Int)]
iter i = flatten $ map (\x -> map (\y -> (x,y)) [0..x-1]) [0..i-1]

flatten :: [[(Int,Int)]] -> [(Int,Int)]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

order :: (Int,Int) -> (Int,Int)
order (x,y) = (max x y, min x y)

-- graph_by_func n f returns a graph with n vertices, where f x y == True <-> x has edge to y
graph_by_func :: Int -> ((Int,Int) -> Bool) -> Graph
graph_by_func n f = Graph $ map (\x -> map (\y -> f (x,y)) [0..x-1]) [0..n-1]

graph_by_set :: Int -> [(Int,Int)] -> Graph
graph_by_set n e = graph_by_func n (\i -> elem i (map order e))

edge :: Graph -> Int -> Int -> Bool
edge (Graph l) x y | x == y = False
edge (Graph l) x y = (l !! (max x y)) !! (min x y)

edges :: Graph -> [(Int,Int)]
edges g = filter (\(x,y) -> edge g x y) $ iter (len g)

len :: Graph -> Int
len (Graph l) = length l

empty :: Int -> Graph
empty n = graph_by_func n (\(x,y) -> False)

clique :: Int -> Graph
clique n = graph_by_func n (\(x,y) -> True)

purerandomlist :: StdGen -> Int -> [Bool]
purerandomlist g x = take x $ randoms g

randomgraph :: IO StdGen -> Int -> IO Graph
randomgraph g i = fmap (\z -> Graph (map (\x -> purerandomlist z x) [0..i-1])) g

-- vcCheck X G returns True, when X is a vertex cover in G
vcCheck :: [Int] -> Graph -> Bool
vcCheck x g = all (\edge -> (elem (fst edge) x) || (elem (snd edge) x)) $ edges g

-- vcSet G returns a minimal vertex cover
-- vcSet :: Graph -> [Int]

main = do
    ((fmap show (randomgraph newStdGen 5)) >>= putStrLn)
