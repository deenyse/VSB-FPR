import Data.Char
import Data.Text.Internal.Fusion (Stream)

simple :: a -> [b] -> [(a, b)]
simple _ [] = []
simple x (y : ys) = (x, y) : simple x ys

dotProduct :: [a] -> [b] -> [(a, b)]
dotProduct [] _ = []
dotProduct (x : xs) ys = simple x ys ++ dotProduct xs ys

dotProduct2 :: [a] -> [b] -> [(a, b)]
dotProduct2 [] _ = []
dotProduct2 xs ys = [(x, y) | x <- xs, y <- ys]

allToUpper2 :: String -> String
allToUpper2 xs = [toUpper x | x <- xs]

oddList :: Int -> Int -> [Int]
oddList a b = [x| x <-[a..b], odd x]

removeAllUpper :: String -> String
removeAllUpper xs = [x| x <- xs, not (isUpper x)]

removeAllUpper2 :: String -> String
removeAllUpper2 = filter (not . isUpper)


union :: Eq a => [a] -> [a] -> [a]
union xs ys  = [x | x<- xs, y<- ys, x /= y]

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys  = [x | x<- xs, y<- ys, x == y]

freequency :: Char -> String -> Int
freequency a s = length(filter (==a) s)

freequency2 :: Char -> String -> Int
freequency2 a s = sum(map(\x -> if x==a then 1 else 0) s)


isPrime :: Int -> Bool
isPrime 1 = False
isPrime y = isPrimeTest y (y-1) where
  isPrimeTest _ 1 = True 
  isPrimeTest n x | n `mod` x ==0 = False
                  | otherwise = isPrimeTest n (x-1)


goldbach :: Int-> [(Int, Int)]
goldbach n = let prvocisla = filter isPrime [1..div n 2]
             in [(p, n-p) | p <- prvocisla, isPrime(n-p)]


goldbachList :: Int -> Int-> Int -> [(Int, Int)]
goldbachList a b lim = filter (\x -> fst x > lim) [head(goldbach p) | p <- [a..b], even p]


type Pic = [String]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]



flipV :: Pic -> Pic
flipV x = map reverse x

flipH :: Pic -> Pic
flipH x = reverse x

above :: Pic -> Pic -> Pic
above p1 p2 = p1 ++ p2

sideBySide :: Pic -> Pic -> Pic
sideBySide (x:xs) (y:ys) = (x++y) : sideBySide xs ys
sideBySide _ _ = []


sideBySide2 :: Pic -> Pic -> Pic
sideBySide2 p1 p2 = zipWith (++) p1 p2


rowToCol :: String -> [String]
rowToCol p1 = [ [x] | x <- p1]


rotateL :: Pic -> Pic
rotateL p1 =[head x | x <- p1] : rotateL [tail x | x <- p1]

rotateR :: Pic -> Pic
rotateR p1 = reverse [head x | x <- p1] : rotateR [tail x | x <- p1]