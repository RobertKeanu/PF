sumImp :: [Integer] -> Integer
sumImp l = foldl (+) 0 (filter even l)

sumImp2 :: [Integer] -> Integer
sumImp2 l = sum $ map (^2) $ filter odd l 

verif :: [Bool] -> Bool
verif l = foldr (&&) True l

allVerifies :: (Int->Bool) ->[Int]->Bool
allVerifies f l = foldr (&&) True (map f l)

anyVerifies :: (Int->Bool) ->[Int]->Bool
anyVerifies f l = foldr (||) False (map f l)

verif1 :: (Int -> Bool) -> Int -> Bool
verif1 f x = f x 

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f = foldr(\y xs -> (f y):xs) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x xs -> if p x then x:xs else xs ) []

listToInt :: [Integer] -> Integer
listToInt = foldl s 0
   where s n c = 10 * n + c

rmChar :: Char -> String -> String
rmChar c s = filter(/=c) s

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s)

rmCharsFold :: String -> String -> String
rmCharsFold r s = foldr rmChar s r