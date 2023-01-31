factori :: Int -> [Int]
factori list = [x | x<-[1..list],list`mod`x == 0]

prim :: Int->Bool
prim a = if(length(factori (a)) == 2)
                then True
        else False

numerePrime :: Int -> [Int]
numerePrime a = [x | x<-[2..a],prim x]


myzip3:: [Int]->[Int]->[Int]->[(Int,Int,Int)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (h1:t1)(h2:t2)(h3:t3) = (h1,h2,h3) : (myzip3 t1 t2 t3)

firstEl :: [(a,b)] -> [a]
firstEl l = map(\x -> fst x) l

sumList :: [[Int]] -> [Int]
sumList l = map(\x -> sum x) l

prel22 :: [Int] -> [Int]
prel22  = map(\x -> if even x then x `div` 2 else 2*x)

ex :: Char->[String]->[String]
ex c s = filter (elem c) s

ex2 :: [Int] -> [Int]
ex2 l = map(\x->x*x)[x|x<-l,x`mod`2/=0]

ex3 :: [Int] -> [Int]
ex3 l = map(\(a,b)->a*a)(filter(\(a,b)-> odd b) (zip l [0..]))

numaiVocale :: [String] ->  [String]
numaiVocale l = map(\x -> filter (\c->elem c "aeiouAEIOU")x)l

mymap :: (a->b) -> [a] -> [b]
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a->Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) 
    |f x = x : (myfilter f xs)
    |otherwise = myfilter f xs