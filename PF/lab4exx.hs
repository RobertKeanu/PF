factori :: Int -> [Int]
factori x = [d | d<-[1..x],x`mod`d == 0]

prim :: Int -> Bool
prim x = if(x==1 || x==0 || length(factori(x))>2) then False else True

numerePrime:: Int-> [Int]
numerePrime x = [d| d<-[2..x], prim d == True]

myzip3:: [Int]->[Int]->[Int]->[(Int,Int,Int)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (h1:t1)(h2:t2)(h3:t3) = (h1,h2,h3) : (myzip3 t1 t2 t3)

--myzip33 :: [Int]->[Int]->[Int]->[(Int,Int,Int)]
--myzip33 l1 l2 l3 = [(x,y,z)|i<-[0..((min(length l1)(min(length l2)(length l3))-1)],let x = l1 !! i, let y = l2 !! i, let z =l3 !! i]

myzip333 :: [Int]->[Int]->[Int]->[(Int,Int,Int)]
myzip333 a b c = if(length a==0 || length b ==0 || length c==0) then []
                else (head a, head b, head c): myzip333(tail a)(tail b)(tail c)

myzip3333:: [Int]->[Int]->[Int]->[(Int,Int,Int)]
myzip3333 a b c = [(x,y,z) | ((x,y),z)<-zip(zip a b)c]

firstEl::[(a,b)]->[a]
firstEl x = [fst(a,b) | (a,b) <- x]

firstEll::[(a,b)]->[a]
firstEll l = map(\x-> fst x) l

sumList :: [[Int]] -> [Int]
sumList l = map sum l
--fara list comprehension

prel2 :: [Int]->[Int]
prel2 l = map(*2) (filter (\x->odd x) l ) ++ map(`div`2)(filter(\x->even x)l)

prel22 :: [Int] -> [Int]
prel22  = map(\x -> if even x then x `div` 2 else 2*x)

ex :: Char->[String]->[String]
ex c s = filter (c `elem`) s

ex2 :: [Int] -> [Int]
ex2 l = map(\x->x*x)(filter (\x->odd x) l)

ex3 :: [Int] -> [Int]
ex3 l = map(\(a,b)->a*a)(filter(\(a,b)-> odd b) (zip l [0..]))

numaiVocale :: [String] -> [String]
numaiVocale l = map(\s->filter(\c-> elem c "aeiouAEIOU")s)l

mymap :: (a->b) -> [a] -> [b]
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a->Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) 
    |f x = x : (myfilter f xs)
    |otherwise = myfilter f xs