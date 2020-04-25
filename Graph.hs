module Graph where

data Graph = Graph [[Bool]]

edge :: Graph -> Int -> Int -> Bool
edge (Graph l) x y = (l !! (max x y)) !! (min x y)
