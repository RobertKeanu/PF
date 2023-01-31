import Data.Char
palindrom :: String -> Bool
palindrom a = if(a == reverse a)
                then True
            else False

voc :: Char -> Integer
voc a = if(elem a "aeiouAEIOU")
            then 1
        else 0

nrVoc :: [Char] ->Integer
nrVoc [] = 0
nrVoc (h:t) = (voc h) + nrVoc t

nrVocale :: [String] -> Integer
nrVocale [] = 0
nrVocale l = if(head l  == reverse (head l))
                then nrVoc (head l) + nrVocale (tail l)
            else nrVocale(tail l)

elementt :: Int -> [Int] -> [Int]
elementt n [] = []
elementt n l = (head l) : (if (even (head l)) then n : elementt n (tail l)
                          else elementt n (tail l))

divz :: Int -> [Int]
divz a = [x | x <- [1..a] , a `mod` x == 0]

listadiv :: [Int] -> [[Int]]
listadiv lis = [divz a | a <-lis]

inIntervalComp :: Integer -> Integer -> [Integer] -> [Integer]
inIntervalComp a b nr = [x | x<-nr , x >= a && x<=b]

inIntervalRec :: Integer -> Integer -> [Integer] -> [Integer]
inIntervalRec a b [] = []
inIntervalRec a b nr = if(head nr >= a && head nr <=b)
                            then(head nr) : inIntervalRec a b (tail nr)
                        else inIntervalRec a b (tail nr)

pozitiveComp :: [Integer] -> Integer
pozitiveComp lis = sum[1 | x<-lis, x>0]

pozitiveRec :: [Integer] -> Integer
pozitiveRec [] = 0
pozitiveRec lis = if(head lis>0)
                    then 1+pozitiveRec(tail lis)
                else pozitiveRec(tail lis)

pozitiiImpareComp :: [Integer] -> [Integer]
pozitiiImpareComp lis = [poz | (elemt,poz)<-zip lis[0..],odd elemt]

pAuxRec :: [Integer] -> Integer -> [Integer]
pAuxRec [] _ = []
pAuxRec (h:t) i 
    |odd h = i : pAuxRec t(i+1)
    |otherwise = pAuxRec t(i+1)

pozitiiImpareRec :: [Integer] -> [Integer]
pozitiiImpareRec l = pAuxRec l 0

-- functiile isDigit si digitToInt exista deja, in libraria Data.Char
-- ti-am mutat si codul de la celelalte laboratoare
--ok , mersi!

multDigitsComp :: String -> Int
multDigitsComp lis = product[digitToInt(x) | x<-lis, isDigit(x)]

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec s = if(isDigit(head s))
                    then (ord(head s) - ord '0') * multDigitsRec(tail s)
                else multDigitsRec(tail s)