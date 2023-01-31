-- primele lab-uri
import Data.Char

isVowel :: Char -> Int
isVowel ch = if (elem ch "aeiouAEIOU")
                then 1
             else 0

nrVocaleCuvant :: [Char] -> Int
nrVocaleCuvant [] = 0
nrVocaleCuvant (h : t) = (isVowel h) + nrVocaleCuvant t 

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale l = if (head l == reverse (head l)) 
                then nrVocaleCuvant (head l) + nrVocale (tail l)
             else nrVocale (tail l)



addElem :: Int -> [Int] -> [Int]
addElem n [] = []
addElem n l = (head l) : (if (even (head l)) then n : addElem n (tail l)
                          else addElem n (tail l))



divizori :: Int -> [Int]
divizori n = [x | x <- [1..n], n `mod` x == 0]



listadiv :: [Int] -> [[Int]]
listadiv nr = [divizori x | x <- nr]




inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b nr = if (head nr >= a && head nr <= b) 
                           then (head nr) : inIntervalRec a b (tail nr)
                       else inIntervalRec a b (tail nr) 

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b nr = [x | x <- nr, a <= x, b >= x]



pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec nr = if (head nr > 0) 
                then 1 + pozitiveRec (tail nr)
            else pozitiveRec (tail nr)

pozitiveComp :: [Int] -> Int
pozitiveComp nr = length [x | x <- nr, x > 0]



pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp nr = [y | (x, y) <- (zip nr [0..]), x `mod` 2 == 1]

aux :: [Int] -> Int -> [Int]
aux [] n = []
aux nr n = if ((head nr) `mod` 2 == 1)
                then n : aux (tail nr) (n + 1)
            else aux (tail nr) (n + 1)
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec nr = aux nr 0



multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec s = if (isDigit (head s))
                     then (ord (head s) - ord '0') * multDigitsRec (tail s)
                  else multDigitsRec (tail s)

multDigitsComp :: String -> Int
multDigitsComp s = product [digitToInt x | x <- s, isDigit x]


firstEl :: [(a, b)] -> [a]
firstEl l = map fst l



myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 l1 l2 l3 = [(x, y, z) | i <- [0..((min (min (length l1) (length l2)) (length l3))-1)], let x = l1 !! i, let y = l2 !! i, let z = l3 !! i]
--[(x, y, z) | ((x, y), z) <- zip (zip a b) c]



sumlist :: [[Int]] -> [Int]
sumlist l = map sum l


prel2 :: [Int] -> [Int]
prel2 l = map (\x -> (if (x `mod` 2 == 0) then x `div` 2 else x * 2)) l


ex8 :: Char -> [String] -> [String]
ex8 ch l = filter (\x -> ch `elem` x) l


ex9 :: [Int] -> [Int]
ex9 l = map (^2) (filter (\x -> x `mod` 2 == 1) l)



ex10 :: [Int] -> [Int]
ex10 l = map (^2) (map fst (filter (\x -> (snd x) `mod` 2 == 1) (zip l [1..(length l)])))



ex11 :: [String] -> [String]
ex11 l = map (\s -> filter (\ch -> ch `elem` "aeiouAEIOU") s) l



mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f l = f (head l) : mymap f (tail l)


myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f l = if (f (head l)) then ((head l) : myfilter f (tail l))
               else (myfilter f (tail l))




sum_patrate :: [Int] -> Int
sum_patrate l = foldr (+) 0 (map (^2) (filter (\x -> x `mod` 2 == 1) l))



is_true :: [Bool] -> Bool
is_true l = foldr (==) True l



allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies op l = (length l) == (length (filter op l))



anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies op l = (length (filter op l)) > 0



mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr op l = foldr (\h t -> (op h) : t) [] l



filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr op l = foldr (\h t -> if ((op h == True)) then (h : t) else t) [] l



listToInt :: [Integer] -> Integer
listToInt l = foldl (\h t -> (h * 10 + t)) 0 l



rmChar :: Char -> String -> String
--rmChar ch s = foldr (\h t -> if (h == ch) then t else (h : t)) [] s
rmChar ch s = filter (/= ch) s



rmCharsRec :: String -> String -> String
rmCharsRec [] b = b
rmCharsRec a b = rmCharsRec (tail a) (foldr (\h t -> (rmChar (head a) (h : t))) [] b)



rmCharsFold :: String -> String -> String
rmCharsFold a b = foldr rmChar b a


myreplicate :: Int -> Double -> [Double]
myreplicate 0 v = []
myreplicate n v = [v] ++ myreplicate (n - 1) v               



sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h : t) = if (h `mod` 2 == 0) 
                    then sumImp t                
                    else h + sumImp t
                    
totalLen :: [String] -> InttotalLen [] = 0totalLen (h : t) = if (h !! 0 == 'A') 
                      then length h + totalLen t                   
                      else totalLen t
                      
                      
                      
-- lab 6

data Fruct = Mar String Bool | Portocala String Int


ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala nume nr) = elem nume ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False


nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum [nr | (Portocala nume nr) <- (filter ePortocalaDeSicilia l)]


nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length (filter (\(Mar nume viermi) -> viermi) l)


type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
        deriving Show


vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"


rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r


data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show


verifica :: Matrice -> Int -> Bool
verifica (M m) n = (foldr (\(L l) ls -> ((sum l) == n) : ls) [] m) == [True | i <- [1..(length m)]]


doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = length (filter (\(L l) -> (length l) == n) m) == 
                   length (filter (\(L l) -> (length l) == n && length (filter (>0) l) == n) m)


corect :: Matrice -> Bool
corect (M ((L h) : [])) = True
corect (M ((L h1) : (L h2) : t)) = if (length h1) == (length h2)
                                    then corect (M ((L h2) : t))
                                   else False




-- lab 7 8

data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (e1 :*: e2) = show e1 ++ "*" ++ show e2


evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x y) = evalArb x + evalArb y
evalArb (Node Mult x y) = evalArb x * evalArb y



expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)



class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value

  keys x = [key | (key, _) <- toList x]
  values x = [value | (key, value) <- toList x]
  fromList [] = empty
  fromList ((key, value) : t) = insert key value (fromList t)


newtype PairList k v
  = PairList { getPairList :: [(k, v)] }


instance Collection PairList where
        empty = PairList []
        singleton k v = PairList [(k, v)] 
        insert k v l = PairList $ (k, v) : (getPairList (delete k l))
        clookup k (PairList l) = lookup k l 
        delete k (PairList l) = PairList (filter (\(key, val) -> key /= k) l)
        toList (PairList l) = l



data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare



instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v Empty = singleton k v
    insert k v (BNode left key (value) right)
            | k < key = BNode (insert k v left) key value right
            | k > key = BNode left key value (insert k v right)
            | otherwise = BNode left key (Just v) right 
    clookup k Empty = Nothing
    clookup k (BNode left key value right) 
            | k < key = clookup k left
            | k > key = clookup k right 
            | otherwise = value
    delete k (BNode left key value right)
            | k < key = delete k left 
            | k > key = delete k right 
            | otherwise = BNode left key Nothing right
    toList (BNode left key Nothing right) = toList left ++ toList right
    toList (BNode left key (Just value) right) = toList left ++ toList right ++ [(key, value)]


data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
            deriving Show

class ToFromArb a where
        toArb :: a -> Arb
        fromArb :: Arb -> a


instance Show Punct where
        show (Pt []) = "()"
        show (Pt (h : t)) = "(" ++ show h ++ (foldr (++) "" (map (\x -> ", " ++ show x) t)) ++ ")"


getList :: Punct -> [Int]
getList (Pt []) = []
getList (Pt (h : t)) = h : getList (Pt t)

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt [a]) = F a 
    toArb (Pt (h : t)) = N (F h) (toArb (Pt t))
    fromArb Vid = Pt[]
    fromArb (F a) = Pt [a]
    fromArb (N left right) = Pt (getList (fromArb left) ++ getList(fromArb right))


data Geo a = Square a | Rectangle a a | Circle a
            deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a


instance GeoOps Geo where
    perimeter (Square a) = 4 * a
    perimeter (Rectangle a b) = 2 * a + 2 * b 
    perimeter (Circle a) = 2 * pi * a
    area (Square a) = a * a;
    area (Rectangle a b) = a * b 
    area (Circle a) = pi * a * a

instance (Eq x, Floating x) => Eq (Geo x) where 
    a == b = perimeter a == perimeter b
 

-- lab 9

import Data.Maybe (fromJust)
import Data.List

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop 
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q")))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: 
     ((Not (Var "P")) :|: (Not (Var "Q")) :&:
     (Not (Var "P")) :|: (Not (Var "R")))


instance Show Prop where
    show (Var nume) = nume
    show F = "F"
    show T = "T"
    show (Not p) = "(~" ++ show p ++ ")"
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"

-- ai ratat F si T
--e ok?
-- da, perfect!
--mersi!
type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var p) e = impureLookup p e
eval F e = False
eval T e = True
eval (Not p) e = not (eval p e)
eval (p :|: q) e = (eval p e) || (eval q e) 
eval (p :&: q) e = (eval p e) && (eval q e) 
-- (p -> q) <=> (!p || q)  
eval (p :->: q) e = (eval (Not p) e) || (eval q e) 
eval (p :<->:q) e = (eval (p :->: q) e) && (eval (q :->: p) e)

-- poate fi Not (expresie), rezolva la cazul general
-- si cred ca iti mai tb niste paranteze unde ai (eval p e)
--mersi!!


variabile :: Prop -> [Nume]
variabile (Var p) = [p]
variabile (Not p) = variabile p 
variabile (p :|: q) = nub (variabile p ++ variabile q)
variabile (p :&: q) = nub (variabile p ++ variabile q)
variabile (p :->: q) = nub (variabile p ++ variabile q)
variabile (p :<->: q) = nub (variabile p ++ variabile q)

envs :: [Nume] -> [Env]
envs [] = []
envs [t] = [[(t, False)], [(t, True)]]
envs (h : t) = [(h, False) : x | x <- (envs t)] ++ [(h, True) : x | x <- (envs t)]

satisfiabila :: Prop -> Bool
satisfiabila p = length (filter (\x -> x == True) [eval p e | e <- envs (variabile p)]) > 0

valida :: Prop -> Bool
valida p = satisfiabila (Not p) == False

echivalenta :: Prop -> Prop -> Bool
echivalenta p q = valida (p :<->: q)


-- lab 10

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)


data Constant a b = Constant b

instance Functor (Constant a) where 
    fmap f (Constant b) = Constant (f b)


data Two a b = Two a b

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b)


data Three a b c = Three a b c 

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)


data Three' a b = Three' a b b

instance Functor (Three' a) where 
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)


data Four a b c d = Four a b c d

instance Functor (Four a b c) where 
    fmap f (Four a b c d) = Four a b c (f d)


data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where 
    fmap f (Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where 
    fmap f Finance = Finance 
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where 
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)


data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where 
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt 
    fmap f (Print s a) = Print s (f a)
    fmap f (Read sa) = Read (fmap f sa)




-- lab 11

data List a = Nil
              | Cons a (List a)
        deriving (Eq, Show)



instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

concat' :: List a -> List a -> List a
concat' Nil a = a 
concat' a Nil = a 
concat' (Cons a b) c = Cons a (concat' b c)

instance Applicative List where
    pure a = Cons a Nil
    f <*> Nil = Nil
    Nil <*> f = Nil
    (Cons a la) <*> (Cons b lb) = Cons (a b) ((concat' (fmap a lb)) (la <*> (Cons b lb)))


data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty [] = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative x = if (x < 0) then Nothing else (Just x)

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString s x y = if (noEmpty s /= Nothing && noNegative x /= Nothing && noNegative y /= Nothing)
                            then (Just (Cow s x y))
                      else Nothing

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 s x y = Cow <$> (noEmpty s) <*> (noNegative x) <*> (noNegative y)



newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x s = if (length s >= x) then Nothing else (Just s) 

mkName :: String -> Maybe Name
mkName s = if (validateLength 25 s /= Nothing) then (Just (Name s))
           else Nothing

mkAddress :: String -> Maybe Address
mkAddress s = if (validateLength 100 s /= Nothing) then (Just (Address s))
           else Nothing

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = if (mkName nume /= Nothing && mkAddress adresa /= Nothing) 
                            then (Just (Person (Name nume) (Address adresa)))
                       else Nothing

mkName2 :: String -> Maybe Name
mkName2 s = pure Name <*> validateLength 25 s

mkAddress2 :: String -> Maybe Address
mkAddress2 s = pure Address <*> validateLength 100 s

mkPerson2 :: String -> String -> Maybe Person
mkPerson2 nume adresa = pure Person <*> mkName2 nume <*> mkAddress2 adresa




-- lab 12

import Data.Monoid

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x l = foldr (\h t -> h == x || t) False l

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 x l = getAny $ foldMap (\h -> Any(x == h)) l

null1 :: (Foldable t) => t a -> Bool
null1 l = foldr (\h t -> False) True l

null2 :: (Foldable t) => t a -> Bool
null2 l = getAll $ foldMap (\h -> All(False)) l

length1 :: (Foldable t) => t a -> Int
length1 l = foldr (\h x -> 1 + x) 0 l

length2 :: (Foldable t) => t a -> Int
length2 l = getSum $ foldMap (\h -> Sum(1)) l

toList1 :: (Foldable t) => t a -> [a]
toList1 l = foldr (:) [] l 

--nu sunt sigura
-- e bine
--mersi!!
toList2 :: (Foldable t) => t a -> [a]
toList2 l = foldMap (\h -> [h]) l

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 x = foldMap (\h -> h) x


data Constant a b = Constant b

instance Foldable (Constant a) where 
    foldMap f (Constant b) = f b 

data Two a b = Two a b

instance Foldable (Two a) where 
    foldMap f (Two a b) = f b 

data Three a b c = Three a b c

instance Foldable (Three a b) where 
    foldMap f (Three a b c)  = f c 

data Three' a b = Three' a b b

instance Foldable (Three' a) where 
    foldMap f (Three' a b1 b2) = f b1 <> f b2

data Four' a b = Four' a b b b

instance Foldable (Four' a) where 
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3 

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable (GoatLord) where 
    foldMap f NoGoat = mempty
    foldMap f (OneGoat a) = f a 
    foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c
     


-- lab 13

pos :: Int -> Bool
pos x = if (x>=0) then True else False

fct :: Maybe Int -> Maybe Bool
fct mx = mx >>= (\x -> Just (pos x))

fct' mx = do {
        x <- mx;
        return (pos x);
}

addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do {
        x <- mx;
        y <- my;
        return (x + y);
}

cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product' mxs mys = do {
        xs <- mxs;
        ys <- mys;
        return (xs, ys);
}

prod f xs ys = [f x y | x <- xs, y<-ys]
prod' f xs ys = do {
        x <- xs;
        y <- ys;
        return (f x y);
}

myGetLine :: IO String
myGetLine = getChar >>= \x ->
                if x == '\n' then
                    return []
                else
                    myGetLine >>= \xs -> return (x:xs)
                    
myGetLine' :: IO String
myGetLine' = do {
    x <- getChar;
    if x == '\n' then
        return [];
    else 
        do {
        xs <- myGetLine';
        return (x : xs);
    }
}


prelNo noin = sqrt noin
ioNumber = do
    noin <- readLn :: IO Float
    putStrLn $ "Intrare\n" ++ (show noin)
    let noout = prelNo noin
    putStrLn $ "Iesire"
    print noout

ioNumber' = (readLn :: IO Float) >>= \x -> (putStrLn $ "Intrare\n" ++ (show x)) >> (let n = (prelNo x) in (putStrLn "Iesire") >> print n)


newtype WriterS a = Writer {runWriter :: (a, String)}

instance Monad WriterS where
  return va = Writer (va, "")
  ma >>= k =
    let (va, log1) = runWriter ma
        (vb, log2) = runWriter (k va)
     in Writer (vb, log1 ++ log2)

instance Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor WriterS where
  fmap f ma = pure f <*> ma

tell :: String -> WriterS ()
tell log = Writer ((), log)

logIncrement :: Int -> WriterS Int
logIncrement x = do
  tell ("increment:" ++ show x ++ "\n")
  return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
  y <- logIncrement x
  logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
  if n > 0
    then do
      a <- logIncrement x
      logIncrementN a (n - 1)
    else return x

data Person = Person {name :: String, age :: Int}

showPersonN :: Person -> String
showPersonN (Person name age) = "NAME:" ++ show name

showPersonA :: Person -> String
showPersonA (Person name age) = "AGE:" ++ show age

showPerson :: Person -> String
showPerson (Person name age) = showPersonN (Person name age) ++ ", " ++ showPersonA (Person name age)

newtype Reader env a = Reader {runReader :: env -> a}

instance Monad (Reader env) where
  return :: a -> Reader env a
  return x = Reader (\_ -> x)

  (>>=) :: Reader env a -> (a -> Reader env b) -> Reader env b
  ma >>= k = Reader f
    where
      f env =
        let a = runReader ma env
         in runReader (k a) env

instance Applicative (Reader env) where
  pure :: a -> Reader env a
  pure = return

  (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap :: (a -> b) -> Reader env a -> Reader env b
  fmap f ma = pure f <*> ma

ask :: Reader env env
ask = Reader id

mshowPersonN :: Reader Person String
mshowPersonN = do
  env <- ask
  return ("NAME: " ++ name env)

mshowPersonA :: Reader Person String
mshowPersonA = do
  env <- ask
  return ("AGE: " ++ show (age env))

mshowPerson :: Reader Person String
mshowPerson = do
  env <- ask
  return ("(NAME: " ++ name env ++ ", AGE: " ++ show (age env) ++ ")")


