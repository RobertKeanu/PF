maxim3 :: Int -> Int -> Int -> Int
maxim3 a b c = if (a>b && a>c)
                    then a
                else if (a>b && a<c)
                    then c
                else b

maxim :: Int->Int->Int
maxim x y = if(x>y)
                then x
            else y

maxim4 :: Int->Int->Int->Int->Int
maxim4 a b c d = let t = (maxim3 a b c) in (maxim t d)

patrate :: Int->Int->Int
patrate a b = a*a+b*b

para :: Int->[Char]
para a = if (odd a)
            then "impar"
        else 
            "par"

factorial :: Int->Int
factorial a = if (a==0)
                then 1
            else 
                a * factorial (a-1)

dublu :: Int -> Int -> [Char]
dublu a b = if(a > 2*b)
                then "DA"
            else "NU"