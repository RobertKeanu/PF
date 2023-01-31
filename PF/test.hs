import System.Win32 (xBUTTON1, wAIT_ABANDONED, mAXIMUM_ALLOWED)
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Applicative.Lift (Lift)
import Data.Monoid

ex1 :: [Int] -> Int
ex1 lis = sum (filter odd lis) --sau filter

ex2 :: [Int] -> Int
ex2 lis = sum (map (^2) (filter odd lis))

ex3 :: [Bool] -> Bool
ex3 lis = foldr (&&) True lis

allVerifies :: (Int->Bool) -> [Int] -> Bool
allVerifies f lis = foldr (&&) True (map f lis)

anyVerifies :: (Int->Bool) ->[Int]->Bool
anyVerifies f l = foldr (||) False (map f l)

verifyProperty :: (a -> Bool) -> [a] -> Bool
verifyProperty p xs = foldr (\x acc -> p x && acc) True xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

rmChar :: Char -> String -> String
rmChar c s = filter (/=c) s

rmCharsRec :: String -> String -> String
rmCharsRec [] s2 = s2
rmCharsRec (x:xs) s2 = rmCharsRec xs (rmChar x s2)

data Fruct = Mar String Bool | Portocala String Int

-- -{ionatanFaraVierme = Mar "Ionatan" False
-- goldenCuVierme = Mar "Golden Delicious" True
-- portocalaSicilia10 = Portocala "Sanguinello" 10
-- listaFructe = [Mar "Ionatan" False,
--                 Portocala "Sanguinello" 10,
--                 Portocala "Valencia" 22,
--                 Mar "Golden Delicious" True,
--                 Portocala "Sanguinello" 15,
--                 Portocala "Moro" 12,
--                 Portocala "Tarocco" 3,
--                 Portocala "Moro" 12,
--                 Portocala "Valencia" 2,
--                 Mar "Golden Delicious" False,
--                 Mar "Golden" False,
--                 Mar "Golden" True

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala nume _) = nume `elem` ["Tarocco","Moro","Sanguinello"]
ePortocalaDeSicilia (Mar _ _) = False

getFelii :: Fruct -> Int
getFelii (Mar _ _) = 0
getFelii (Portocala _ nr) = nr

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs) = if ePortocalaDeSicilia x 
                            then getFelii(x) + nrFeliiSicilia xs 
                        else nrFeliiSicilia xs

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs) = if infested x 
                        then 1 + nrMereViermi xs 
                        else nrMereViermi xs
                         where 
                            infested :: Fruct -> Bool 
                            infested (Mar _ True) = True
                            infested (Mar _ False) = False
                            infested (Portocala _ _) = False

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ ras) = Just ras
rasa (Pisica _) = Nothing

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M m) n = foldr(\(L l) curent -> curent && (sum(l) == n)) True m 

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10

ispozitiv :: [Int] -> Bool
ispozitiv line = foldr(\element current -> current && (element > 0)) True line

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = foldr(\(L l) curent -> curent && (length(l) == n && ispozitiv (l))) True m

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3

testPoz3 = doarPozN (M [L[1,2,3], L[1,2,3], L[2,3,4]]) 3

checklist :: Matrice -> [Int]
checklist (M m) = map (\(L l) -> length (l)) m

corect :: Matrice -> Bool
corect m = all (\element -> element == head(checklist m)) (checklist m)

testP3 = corect (M [L[1,2,3], L[1,2,3], L[2,3,4]])

data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

eval1 :: Tree -> Int
eval1 (Lf n) = n    
eval1 (Node Add st dr) = eval1 st + eval1 dr 
eval1 (Node Mult st dr) = eval1 st * eval1 dr

eval2 :: Expr -> Int
eval2 (Const x) = x 
eval2 (m :+: n) = eval2 m + eval2 n     
eval2 (n :*: m) = eval2 m * eval2 n     

eval3 :: Expr -> Tree
eval3 (Const x) = Lf x  
eval3 (m :+: n) = Node Add (eval3 m) (eval3 n)  
eval3 (m :*: n) = Node Mult (eval3  m) (eval3  n)     

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

-- data Punct = Pt [Int]

-- data Arb = Vid | F Int | N Arb Arb
--           deriving Show

-- class ToFromArb a where
--  	    toArb :: a -> Arb
-- 	    fromArb :: Arb -> a


-- instance Show Punct where
--     show (Pt []) = "()"
--     show (Pt (x:xs)) = "(" ++ show x ++ concat [b : show a | (a,b) <- zip xs [',',','..]] ++ ")"

-- fun :: Punct -> [Int]
-- fun (Pt []) = []
-- fun (Pt (x:xs)) = x : fun(Pt xs)

-- instance ToFromArb Punct where
--     fromArb Vid = Pt []
--     fromArb (F t) = Pt [t]
--     fromArb (N x xs) = Pt(fun(fromArb x) ++ fun(fromArb xs))

--     toArb (Pt []) = Vid
--     toArb (Pt [t]) = F t
--     toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))


type Nume = String

type Env = [(Nume, Bool)]

envs :: [Nume] -> [Env]
envs l = [zip l e |e <- sequence (take (length l) (repeat [True,False]))] 

data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval F _ = False
eval T _ = True
eval (Not a) b = not(eval a b)
eval (Var a) b = impureLookup a b
eval (a :|: b) c = (eval a c) || (eval b c)
eval (a :&: b) c = (eval a c) && (eval b c)
eval (a :->: b) e = not (eval a e) || eval b e
eval (a :<->: b) e = eval (a :->: b) e && eval (b :->: a) e

newtype Identity a = Identity a
instance Functor Identity where
    fmap f(Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
    fmap f(Pair a1 a2) = Pair (f a1) (f a2)
    
data Constant a b = Constant b
instance Functor (Constant a) where
    fmap f (Constant b1) = Constant (f b1)

data Two a b = Two a b
instance Functor (Two a) where 
    fmap f (Two a b1) = Two a (f b1)

data Three a b c = Three a b c
instance Functor (Three a b) where  
    fmap f(Three a1 b1 c1) = Three a1 b1 (f c1)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a1 b b1) = Three' a1 (f b) (f b1)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f (Finance) = Finance  
    fmap f(Desk a) = Desk a     
    fmap f(Bloor b) = Bloor (f b)  

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut f1) = LiftItOut (fmap f f1)    

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f,Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa f1 g1) = DaWrappa (fmap f f1) (fmap f g1)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething g1 g2) = IgnoringSomething g1 (fmap f g2)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where 
    fmap f (Notorious g1 g2 g3) = Notorious g1 g2 (fmap f g3)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where 
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where 
    fmap f Halt = Halt 
    fmap f (Print x a) = Print x (f a)
    fmap f (Read f1) = Read (fmap f f1)

data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil 
    fmap f(Cons x xs) = Cons (f x) (fmap f xs)

concatenare :: List a -> List a -> List a
concatenare a Nil = a
concatenare Nil a = a
concatenare (Cons a l) b = Cons a (concatenare l b)

instance Applicative List where 
    pure x = Cons x Nil 
    Nil <*> x = Nil 
    x <*> Nil = Nil 
    Cons f lis <*> (Cons f1 lis1) = Cons (f f1) (concatenare (fmap f lis1) (lis <*> Cons f1 lis1))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty str = if(str == [])
                    then Nothing 
                else Just str 

noNegative :: Int -> Maybe Int 
noNegative a = if (a < 0)
                then Nothing 
            else Just a 

cowFromString :: String -> Int -> Int -> Maybe Cow 
cowFromString str a b = if(noEmpty(str) /= Nothing && noNegative(a) /= Nothing && noNegative(b) /= Nothing)
                                then Just (Cow{name = str , age = a, weight = b})
                            else Nothing

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address 
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String 
validateLength x str = if ((length(str)) < x)
                            then Just str 
                        else Nothing

mkName :: String -> Maybe Name
mkName str = if ((validateLength 25 str) /= Nothing)
                then Just (Name str)
            else Nothing

mkAddress :: String -> Maybe Address
mkAddress str = if ((validateLength 100 str) /= Nothing)
                then Just (Address str)
            else Nothing

mkPerson :: String -> String -> Maybe Person 
mkPerson str1 str2 = if(((mkAddress str1) /= Nothing) && ((mkAddress str2) /= Nothing))
                            then Just (Person (Name str1) (Address str2)) 
                        else Nothing

mkPerson2 :: String -> String -> Maybe Person 
mkPerson2 a b = Person <$> Just (Name a) <*> Just (Address b)

mkAddress2 :: String -> Maybe Address
mkAddress2 adresa = Address <$> validateLength 100 adresa 

mkName2 :: String -> Maybe Name 
mkName2 nume = Name <$> validateLength 25 nume

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x l = foldr (\h t -> h == x || t) False l

null1 :: (Foldable t) => t a -> Bool
null1 l = foldr (\h t -> False) True l

length1 :: (Foldable t) => t a -> Int
length1 l = foldr (\h t -> 1 + t) 0 l 

-- toList1 :: (Foldable t) => t a -> [a]
-- toList1 l = foldr (:) []

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

-- data Constant a b = Constant b
-- instance Foldable (Constant a) where 
--     foldMap f (Constant b) = f b 

-- data Two a b = Two a b
-- instance Foldable (Two a) where 
--     foldMap f (Two a b) = f b

-- data Three a b c = Three a b c
-- instance Foldable (Three a b) where 
--     foldMap f (Three a b c) = f c

-- data Three' a b = Three' a b b
-- instance Foldable (Three' a) where 
--     foldMap f (Three' a b1 b2) = f b1 <> f b2

-- data Four' a b = Four' a b b b
-- instance Foldable (Four' a) where 
--     foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

-- data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
-- instance Foldable GoatLord where 
--     foldMap f (NoGoat) = mempty
--     foldMap f (OneGoat a) = f a 
--     foldMap f (Moregoats g1 g2 g3) = foldMap f g1 <> foldMap f g2 <> foldMap f g3

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = let Just r = mx;Just b = my in Just (r+b)

addM' mx my = do 
        x <- mx 
        y <- my 
        return Just (x + y)

-- cartesian_product xs ys >>= (\x -> (ys >>= \y -> return (x,y)))

-- cartesian_product1= do
--         t <- xs 
--         z <- ys 
--         return (t,z)

prod f xs ys = [f x y | x<-xs , y<-ys]

prod1 f xs ys = do 
        z <- xs 
        t <- ys 
        return f (z t)

myGetLine :: IO String 
myGetLine = getChar >>= \x -> 
    if x == '\n' then 
        return []
    else 
        myGetLine >>= \xs -> return (x:xs)

myGetLine1 = do 
        x <- getChar 
        if x == '\n' 
            then return [] 
        else do 
            xs <- myGetLine1
            return (x:xs)

prelNo noin =  sqrt noin

ioNumber' = do
    x<-readLn 
    return (prelNo x)

newtype WriterS a = Writer { runWriter :: (a, String) } 


