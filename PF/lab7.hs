data Expr = Const Int
            | Expr :+: Expr
            | Expr :*: Expr
            deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int
          | Node Operation Tree Tree
           deriving (Eq, Show)

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

evalExp :: Expr -> Int
evalExp (Const n)= n
evalExp (n :+: m) = evalExp n + evalExp m 
evalExp (n :*: m) = evalExp n * evalExp m 

evalArb :: Tree -> Int
evalArb (Lf n) = n
evalArb (Node Add n m) = evalArb n + evalArb m 
evalArb (Node Mult n m) = evalArb n * evalArb m

expToArb :: Expr -> Tree
expToArb (Const n) = Lf n
expToArb (n :+: m) = Node Add (expToArb n) (expToArb m)
expToArb (n :*: m) = Node Mult (expToArb n) (expToArb m)

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

  keys x = [fst x | x <- toList x]
  values x = [ snd x | x <- toList x]
  fromList [] = empty
  fromList (x:xs) = (insert (fst x) (snd x) (fromList xs) )

newtype PairList k v = PairList { getPairList :: [(k, v)] } 
   deriving Show

instance Collection PairList where
    empty = PairList[]
    singleton k v = PairList [(k,v)]
    insert k v l = PairList $ (k,v):(getPairList(delete k l))                   -- facem tuplu si adaugam la lista
    clookup k (PairList l) = lookup k l
    delete k (PairList l) = PairList(filter(\(x,y)->x/=k) l)                     --delete k(PairList l)
    toList (PairList l) = l 

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value)


instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v Empty = singleton k v 
    insert k v (BNode left key val right )
        | key < k = BNode left key val (insert k v right)
        | key > k = BNode (insert k v left) key val right
        | otherwise = BNode left key val right
    clookup k Empty = Nothing
    clookup k (BNode left key val right) 
        | key > k = clookup k left
        | key < k = clookup k right
        | otherwise = val
    delete k (BNode left key val right) 
        | key > k = delete k left
        | key < k = delete k right
        | otherwise = BNode left key Nothing right
    toList Empty = []
    toList (BNode left key Nothing right) = toList left ++ toList right
    toList (BNode left key (Just val) right) = toList left ++ [(key,val)]  ++ toList right