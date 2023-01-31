data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a


instance Show Punct where
    show (Pt []) = "()"
    show (Pt (x:xs)) = "("++ show x ++ concat [b: show a | (a,b) <- zip xs [',',','..]] ++ ")"

fun :: Punct -> [Int]
fun (Pt []) = []
fun (Pt (x:xs)) = x : fun(Pt xs)


instance ToFromArb Punct where
    fromArb Vid = Pt []
    fromArb (F t) = Pt [t]
    fromArb (N x xs) = Pt( fun(fromArb x) ++ fun(fromArb xs))

    toArb (Pt []) = Vid
    toArb (Pt [t]) = F t
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

data Geo a = Square a | Rectangle a a | Circle a
  deriving (Show)

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) => g a -> a


instance GeoOps Geo where
    perimeter (Square latura) = 4*latura
    perimeter (Rectangle laturaMare laturaMica) = 2*laturaMare+ 2*laturaMica
    perimeter (Circle r) = 2* pi*r

    area (Square latura) =  latura ^ 2
    area (Rectangle laturaMare laturaMica) = laturaMica * laturaMare
    area (Circle r) = pi * (r^2)

instance (Floating l, Eq l) => Eq (Geo l) where
    x == y = perimeter x == perimeter y
