import Data.Char(digitToInt)
{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

concatenare :: List a -> List a -> List a
concatenare a Nil = a
concatenare Nil a = a
concatenare (Cons a l) b = Cons a (concatenare l b)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> d = Nil
    d <*> Nil  = Nil
    Cons f lis <*> (Cons f1 lis1) = Cons (f f1) (concatenare (fmap f lis1) (lis <*> Cons f1 lis1))

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)


data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty s = if(s == [])
                then Nothing
            else Just s
noNegative :: Int -> Maybe Int
noNegative g = if(g>0)
                then Just g
               else Nothing

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString str a b = if((noEmpty str) /= Nothing && (noNegative a) /= Nothing && (noNegative b) /= Nothing)
                            then Just (Cow {name = str,age = a,weight =b})
                        else Nothing

cowFromString1 :: String -> Int -> Int -> Maybe Cow
cowFromString1 str a b = Cow <$> (noEmpty str) <*> (noNegative a) <*> (noNegative b) 

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a sir = if((length sir) < a)
                            then Just sir
                        else Nothing


test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName str = if ((validateLength 25 str) /= Nothing)
                then Just (Name str)
            else Nothing

mkAddress :: String -> Maybe Address
mkAddress str = if((validateLength 100 str) /= Nothing)
                    then Just (Address str)
                else Nothing

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson str1 str2 = if((validateLength 25 str1) /= Nothing && (validateLength 100 str2) /= Nothing)
                        then Just (Person (Name str1) (Address str2))
                    else Nothing

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))

mkPerson2 :: String -> String -> Maybe Person
mkPerson2 a b = Person <$> Just (Name a) <*> Just (Address b)

mkAddress2 :: String -> Maybe Address
mkAddress2 adresa = Address <$> validateLength 100 adresa

mkName2 :: String -> Maybe Name
mkName2 nume = Name <$> validateLength 25 nume