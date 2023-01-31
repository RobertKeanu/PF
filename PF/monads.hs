instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just a >>= f = f a
  
  
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  


data Either err a = Left err | Right a

instance Monad (Either err) where
  return = Right
  Right va >>= k = k va
  err >>= _ = err
  
  
newtype Writer log a = Writer { runWriter :: (a ,log) }

instance Monad (Writer String) where
  return va = Writer ( va , " " )
  ma >>= k =
      let (va , log1) = runWriter ma
          (vb, log2) = runWriter (k va)
      in Writer ( vb , log1 ++ log2 )
      
      
newtype Reader env a = Reader { runReader :: env −> a }

instance Monad (Reader env) where
  return x = Reader ( \ _ −> x )
  ma >>= k = Reader f
            where
            f env = let va = runReader ma env
            in runReader (k va) env


newtype State state a = State { runState :: state −> (a, state) }

instance Monad (State state) where
  return va = State (\s −> (va, s))
  ma >>= k = State $ \s −> let
                        (va, news) = runState ma s
                        State h = k va
                          in (h news)            


data IO a = IO a

instance Monad IO where
  return = IO
  IO x >>= f = f x