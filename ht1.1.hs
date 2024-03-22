import Data.Array (array)
type Grid = [String]
type Cordinates = (Int, Int)

pp :: Grid -> IO ()
pp x = putStr (concatMap (++"\n") x)

getElementAtIndex :: Int -> String -> Char
getElementAtIndex n array
    | length array <= n  = '*'
    | n < 0 = '*'
    | n == 0 = head array
    | otherwise = getElementAtIndex (n-1) (tail array)

getRowAtIndex :: Int -> Grid -> String
getRowAtIndex n array
    | length array <= n = "*"
    | n < 0 = "*"
    | n == 0 = head array
    | otherwise = getRowAtIndex (n-1) (tail array)

getElementByCordinates :: Cordinates -> Grid -> Char
getElementByCordinates (row, col) grid = getElementAtIndex col (getRowAtIndex row grid)

getCharByCoordinates :: Grid -> Cordinates -> Char
getCharByCoordinates grid (row, col) = getElementAtIndex col (getRowAtIndex row grid)

getSCordinates :: Grid -> Cordinates
getSCordinates grid = (rowWithSCordinate, fst (head (filter (\x -> snd x == 's') (zip [0..] rowWithS))))
    where
        rowWithSCordinate = fst (head (filter (\x -> 's' `elem` snd x) (zip [0..] grid)))
        rowWithS = getRowAtIndex rowWithSCordinate grid

replaceInString :: String -> Int -> Char -> String
replaceInString array row char = map (\x -> if fst x == row then char else snd x) (zip [0..] array)

replaceByCordinates :: Grid -> Cordinates -> Char -> Grid
replaceByCordinates grid (row, col) char = map (\x -> if fst x == row then replaceInString (snd x) col char else snd x) (zip [0..] grid)

doGridManipulation :: Grid -> Char -> Grid
doGridManipulation grid action
    | action == 'u' = if getElementByCordinates (sRow-1, sCol) grid  /= '*' then  replaceByCordinates (replaceByCordinates grid sCordinates ' ') (sRow-1, sCol) 's' else grid
    | action == 'd' = if getElementByCordinates (sRow+1, sCol) grid  /= '*' then  replaceByCordinates (replaceByCordinates grid sCordinates ' ') (sRow+1, sCol) 's' else grid
    | action == 'r' = if getElementByCordinates (sRow, sCol +1) grid  /= '*' then  replaceByCordinates (replaceByCordinates grid sCordinates ' ') (sRow, sCol+1) 's' else grid
    | action == 'l' = if getElementByCordinates (sRow, sCol -1) grid  /= '*' then  replaceByCordinates (replaceByCordinates grid sCordinates ' ') (sRow, sCol-1) 's' else grid
    where 
        sCordinates = getSCordinates grid
        sRow = fst sCordinates
        sCol = snd sCordinates
        

maze :: Grid -> String -> Grid 
maze mazePicture actions
    | length actions == 0 = mazePicture
    | otherwise = maze (doGridManipulation mazePicture (head actions)) (tail actions )
    

sampleInput = ["*********",
               "*s*   * *",
               "* * * * *",
               "* * * * *",
               "*   *   *",
               "******* *",
               "        *",
               "*********"]