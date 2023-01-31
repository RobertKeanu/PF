ex1 :: [Int] -> Int
ex1 l = foldl (+) 0 (filter even l)

exx :: [Int] -> Int
exx l = sum(map (^2) (filter odd l))

ex2 :: [Bool] -> Bool
ex2 l = foldr (&&) True l

allVerifies :: (Int->Bool) ->[Int]->Bool
allVerifies f l = foldr (&&) True (map f l)

anyVerifies :: (Int->Bool) ->[Int]->Bool
anyVerifies f l = foldr (||) False (map f l)

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f = foldr(\y xs -> (f y):xs) []

filterFoldr :: (a->Bool) -> [a] -> [a]
filterFoldr f= foldr(\x xs -> if f x then x:xs else xs)[]

listToInt :: [Integer] -> Integer
listToInt = foldl s 0
        where s n c = 10*n + c


rmChar :: Char->String->String
rmChar c s = filter(/=c) s

rmCharsRec :: String->String->String
rmCharsRec [] s = s
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s)

rmCharsFold :: String->String->String
rmCharsFold c s = foldr rmChar s c