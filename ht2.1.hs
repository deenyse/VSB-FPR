import Data.List

data Node = Node {index::Int, neighbors::[Int] } deriving (Show, Eq)
type Graph = [Node]

graph1 :: Graph
graph1=[
        Node 1 [2,3],
        Node 2 [1],
        Node 3 [1],
        Node 4 [5,6],
        Node 5 [7,4],
        Node 6 [4,7],
        Node 7 [5,6,8],
        Node 8 [7],
        Node 9 []
      ]  

getNodeRelations :: Graph -> Int -> [Int]
getNodeRelations [] _  = []
getNodeRelations ((Node index neighbors):xs) target = if index == target then neighbors else getNodeRelations xs target

-- index neighbours relations graph 
dfs :: Int -> [Int] -> [Int] -> Graph -> [Int]
dfs index [] relations graph = if index `elem` relations then relations else dfs index indexRelations (relations ++ [index]) graph
    where
        indexRelations = getNodeRelations graph index
dfs index (x:xs) relations graph = if index `elem` relations then dfs x xs relations graph else dfs x xs (dfs index indexRelations (relations ++ [index]) graph) graph
    where
        indexRelations = getNodeRelations graph index

--  neighbours relations(includes index) graph 
dfs2 :: [Int] -> [Int] -> Graph -> [Int]
dfs2 [] relations _ = relations
dfs2 (x:xs) relations graph = if x `elem` relations then dfs2 xs relations graph else dfs2 xs (dfs2 xNeighbours (relations ++ [x]) graph) graph
    where
        xNeighbours = getNodeRelations graph x

components::Graph -> [[Int]]
components graph = getRelations graph []
    where 
        getRelations:: Graph -> [[Int]] -> [[Int]]
        getRelations [] relations = relations
        getRelations ((Node index neighbors):xs) relations = if index `elem` concat relations then getRelations xs relations else getRelations xs (relations ++ [dfs2 neighbors [index] graph])