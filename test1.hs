convert :: [(String, Int, Float)] -> [(String, Float)] 
convert array = [(a,fromIntegral b*c) | (a,b,c) <- array] 

replaceByRepeat :: String -> Char -> Int -> String
replaceByRepeat array letter times = concatMap(\x -> if x == letter then replicate times x else [x]) array

change :: [a] -> [(Int,Int)] -> [a]
change arr [] = arr
change arr (x:xs) = change (appplyReverseToArray arr x) xs
    where 
        appplyReverseToArray :: [a] -> (Int,Int) -> [a]
        appplyReverseToArray numberArray (start, range) = map fst outputArray
            where 
                indexedArray = zip numberArray [0..]
                leftPart = takeWhile(\(_,b) -> b < start) indexedArray
                middlePart = reverse(filter (\(_,b) -> b >= start && b < start + range) indexedArray)
                rightPart = dropWhile(\(_,b) -> b < start + range) indexedArray
                outputArray = leftPart ++ middlePart ++ rightPart
        