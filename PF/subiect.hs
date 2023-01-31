data Point = Pt [Int]
            deriving Show

data Arb = Empty | Node Int Arb Arb 
            deriving Show 

class ToFromArb a where 
    toArb :: a -> Arb 
    fromArb :: Arb -> a

function :: Point -> [Int]
function (Pt []) = []
function (Pt (x:xs)) = x : function(Pt xs) 

instance ToFromArb Point where 
    fromArb Empty = Pt []
    fromArb (Node a Empty Empty) = Pt [a]
    fromArb (Node a st dr) = Pt(function(fromArb st) ++ [a] ++ function(fromArb dr)) 

    toArb (Pt []) = Empty 
    toArb (Pt [a]) = Node a Empty Empty 
    toArb (Pt (x:xs)) = Node x (toArb (Pt (filter (<x) xs))) (toArb (Pt(filter (>=x)xs)))

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval n1 n2 lis = [e | e<-lis , e >= n1 && e <= n2]

getFromInterval1 :: Int -> Int -> [Int] -> [Int]
getFromInterval1 _ _ [] = []
getFromInterval1 n1 n2 (x:xs) = if (x >= n1 && x<= n2)
                                    then x : getFromInterval1 n1 n2 xs
                                else getFromInterval1 n1 n2 xs
                        
getFromInterval2 :: Int -> Int -> [Int] -> [Int]
getFromInterval2 n1 n2 l = do 
                        e <- l 
                        if (e >= n1 && e<= n2)
                            then return e 
                        else []

getFromInterval3 :: Int -> Int -> [Int] -> [Int]
getFromInterval3 n1 n2 l = l >>= (\e -> if(e >= n1 && e <= n2) then [e] else [])

newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}

instance Monad (ReaderWriter env) where
  return va = RW (\_ -> (va,""))
  ma >>= k = RW f 
      where f env = let (va, str1) = getRW ma env
                        (vb, str2)  = getRW (k va) env
                    in (vb, str1 ++ str2)

instance Applicative (ReaderWriter env) where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor (ReaderWriter env) where              
  fmap f ma = pure f <*> ma  

            