import Data.Monoid

{-
foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i r ) l

instance Foldable BinaryTree where
    foldr = foldTree

none = Nothing
one = Just 3
tree = Node(Node( Leaf 1) ( Leaf 2) ) (Node ( Leaf 3) ( Leaf 4) )
-}
{-
instance Subgroup Any where
    a <> b = a || b

instance Monoid Any where
    mempty = False
-}

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x l = foldr (\h t -> h == x || t) False l

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 x l = getAny $ foldMap (\h -> Any(x == h)) l

-- Any {getAny = True/False}

null1 :: (Foldable t) => t a -> Bool
null1 l = foldr (\h t -> False) True l

null2 :: (Foldable t) => t a -> Bool
null2 l = getAll $ foldMap (\h -> All(False)) l

length1 :: (Foldable t) => t a -> Int
length1 l = foldr(\h t -> 1 + t) 0 l 

length2 :: (Foldable t) => t a -> Int
length2 l = getSum $ foldMap (\h -> Sum(1)) l

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldr (:) []

toList2 :: (Foldable t) => t a -> [a]
toList2 l = foldMap (\h -> [h]) l

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1  = foldMap id

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b
    -- foldMap f (Constant a b) = f b
    -- foldMap f (Constant b1 b2) = f b1 <> f b2

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

-- exGoat = foldMap Sum (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))
-- exGoat2 = foldr (*) 1 (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord where
    foldMap f (NoGoat) = mempty
    foldMap f (OneGoat a) = f a 
    foldMap f (MoreGoats g1 g2 g3) = foldMap f g1 <> foldMap f g2 <> foldMap f g3
