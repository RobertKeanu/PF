eeny:: Integer->[Char]
eeny a = if(mod a 2== 1)
                then "meeny"
            else
                "eeny"

--eeny: Integer -> String
--eeny = undefined
fizzbuzz :: Integer -> [Char]
fizzbuzz a = if(mod a 3 == 0 && mod a 5 /= 0)
                then "Fizz"
            else
            if(mod a 3 /= 0 && mod a 5 ==0)
                then "Buzz"
            else
            if(mod a 15 == 0)
                then "FizzBuzz"
            else
               " "
-- and este &&
-- dar poti pune direct mod 15
-- ala e un else if

tribonacci::Integer->Integer
tribonacci 0 = 0
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = 
    tribonacci (n-1) + tribonacci (n-2) + tribonacci(n-3)

tribonacci2 :: Integer -> Integer
tribonacci2 n   
    |n<2 = n
    |n == 3 = 2
    | otherwise = tribonacci2 (n-1) + tribonacci2(n-2) + tribonacci2(n-3)


binomial :: Integer -> Integer -> Integer
binomial n k
    |k==0 && n>0 = 1
    |k == 0 && n == 0 = 0
    |n == 0 && k>0 = 0
    |otherwise = binomial(n-1) k + binomial (n-1) (k-1)
-- ce o sa faca la binomial 0 0 ?
-- sau poti doar sa scoti un && (in cazul asta && k > 0)

verifL :: [Int] -> Bool
verifL a = if(mod (length a) 2 == 1)
                then True
            else
                False

takefinal :: [Int] -> Int -> [Int]
takefinal lis n = if(length(lis) > n)
                    then drop(length(lis)-n) lis
                else
                    lis

remove  :: [Int] -> Int -> [Int]
remove lis n = take (n-1) lis ++ drop (n) lis
-- True si False

myreplicate :: Integer -> Integer -> [Integer]
myreplicate 0 y = [ ]
myreplicate x y = y : myreplicate (x-1) y

sumImp :: [Integer] -> Integer
sumImp l = sumImp2 0 l
sumImp2 n l = if l == []
                  then n
                  else let x = head l
                           xs = tail l
                       in if odd x
                              then sumImp2 (n+x) xs
                              else sumImp2 n xs

totalLen :: [String] -> Int
totalLen l = totalLen2 0 l
totalLen2 n l = if l == [] 
                    then n
                    else let x = head l
                             xs = tail l
                           in if x == "A"
                                then totalLen2 (n+length (l)) xs
                                else totalLen2 n xs
totalLen3 :: [String] -> Int
totalLen3 [] = 0
totalLen3 (h:t) = (if head h == 'A' then length h else 0) + totalLen3 t

sumImp3 :: [Integer] -> Integer
sumImp3 [] = 0
sumImp3 (h:t) = (if odd h then h else 0 ) + sumImp3 t
