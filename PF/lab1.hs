sumapatrate :: Integer -> Integer -> Integer
sumapatrate a b = a*a +b*b

paritate :: Integer -> [Char]
paritate a = 
            if(mod a 2== 1)
                then "par"
            else
                "impar"
factorial :: Integer -> Integer
factorial a = 
            if (a == 0)
                then 1
            else
                a * factorial (a-1)

check :: Integer -> Integer -> Bool
check a b = 
            if (a>2*b)
                then True
            else
                False

poly::double->double->double->double->double
poly a b c x = a+x*x+b*x+c
f n 
    | n < 2 = n
    |n==2 = ....
    | otherwise = f(n-1) + f(n-2)
    

f2 a b = 
    f2 (a-1) b + ....








