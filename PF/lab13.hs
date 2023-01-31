pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

fct' mx = do 
    x<-mx
    return(pos x)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = let Just r = mx;Just b = my in Just (r+b)

addM' mx my = do 
    x <- mx
    y <- my
    return Just(x + y)

cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

prod f xs ys = [f x y | x <- xs, y<-ys]

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

cartesian_product' xs ys = do
    x <- xs
    y <- ys
    return (x,y)

prod' f xs ys = do
    x <- xs
    y <- ys
    return f x y

myGetLine' = do
    x<-getChar
    if x == '\n'
        then return []
    else do
        xs <- myGetLine'
        return (x:xs)

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber' = do
    x<-readLn 
    return (prelNo x)

ioNumber2 = (readLn :: IO Float) >>= \noin -> (putStrLn $ "Intrare" ++ (show noin)) >> let noout = prelNo noin in ((putStrLn $ "Iesire") >> print noout)

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log) {-pentru 5.1.2 trebuie sa avem [log]-}
  
logIncrement :: Int  -> WriterS Int
logIncrement x = do
    tell ("increment: " ++ show x ++ "\n")
    return (x+1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y<-logIncrement x
    logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
  if n > 0
    then do
      y <- logIncrement x
      logIncrementN y (n - 1)
    else return x

{-newtype WriterLS a = Writer { runWriter :: (a, [String]) } -}
data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person name age) =  "NAME: " ++ show name

showPersonA :: Person -> String
showPersonA (Person name age) =  "AGE: " ++ show age 

showPerson :: Person -> String
showPerson (Person name age) = "("++showPersonN (Person name age) ++ "," ++ showPersonA (Person name age)++ ")"


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

ask :: Reader env env
ask = Reader id

mshowPersonN ::  Reader Person String
mshowPersonN = do 
            env <- ask
            return ("NAME: " ++ show (name env)) 


mshowPersonA ::  Reader Person String
mshowPersonA = do
            env <- ask
            return ("AGE: " ++ show (age env))


mshowPerson ::  Reader Person String
mshowPerson = do
            env <- ask
            return ("(" ++ (name env) ++ "," ++ show (age env) ++ ")")
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}



   