module Graph where
import System.Random

data Graph = Graph [[Bool]]
    deriving Show

edge :: Graph -> Int -> Int -> Bool
edge (Graph l) x y | x == y = False
edge (Graph l) x y = (l !! (max x y)) !! (min x y)

edges :: Graph -> [(Int,Int)]
edges g = filter (\(x,y) -> edge g x y) $ flatten $ map (\x -> map (\y -> (x,y)) [0..x-1]) [0..(len g)-1]

flatten :: [[(Int,Int)]] -> [(Int,Int)]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

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

-- vcCheck X G returns True, when X is a vertex cover in G
vcCheck :: [Int] -> Graph -> Bool
vcCheck x g = all (\edge -> (elem (fst edge) x) || (elem (snd edge) x)) $ edges g

-- vcSet G returns a minimal vertex cover
-- vcSet :: Graph -> [Int]

main = do
    ((fmap show (randomgraph newStdGen 5)) >>= putStrLn)
