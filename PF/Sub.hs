data Point = Pt [Int]
        deriving Show

data Arb = Empty | Node Int Arb Arb
        deriving Show
        
class ToFromArb a where
        toArb :: a -> Arb
        fromArb :: Arb -> a

fun :: Point -> [Int]
fun (Pt []) = []
fun (Pt (x:xs)) = x : fun(Pt xs)

instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt [a]) = Node a Empty Empty
    toArb (Pt (x:xs)) = Node x (toArb (Pt (filter (<x) xs))) (toArb (Pt (filter (>=x) xs)))

    fromArb Empty = Pt []
    fromArb (Node a Empty Empty) = Pt [a]
    fromArb (Node a st dr) = Pt(fun(fromArb(st)) ++ [a] ++ fun(fromArb(dr)))

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval x y lis = [e | e<- lis, e<=y && e>= x]

getFromInterval2 :: Int -> Int -> [Int] -> [Int]
getFromInterval2 x y lis = do 
                            af <- lis
                            if(af >= x && af <= y)
                                then return af
                            else []

newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}






