data Fruct = Mar String Bool | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]


ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala nume _) = nume `elem` ["Tarocco","Moro","Sanguinello"]
ePortocalaDeSicilia(Mar _ _) = False

getfelii :: Fruct -> Int
getfelii (Mar _ _ ) = 0
getfelii (Portocala _ nr) = nr

nrFeliiSicilia :: [Fruct]->Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs) = if ePortocalaDeSicilia x
                            then getfelii x + nrFeliiSicilia xs
                        else nrFeliiSicilia xs

verifmar :: Fruct -> Bool
verifmar (Mar _ t) = t
verifmar (Portocala _ _) = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs) = if verifmar x then 1 + nrMereViermi xs else nrMereViermi xs

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ k) = Just k

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

sumalinii :: Matrice -> [Int]
sumalinii (M []) = []
sumalinii (M (L x:xs)) = sum x : sumalinii (M xs)

verifica :: Matrice -> Int -> Bool
verifica (M m) n = foldr(\ (L l) curent -> curent && (sum (l) == n)) True m

pozitiv :: [Int] -> Bool
pozitiv l = foldr (\element curent-> curent && (element > 0) ) True l

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n= and [ pozitiv l | (L l) <- m, length l ==n ]


corect :: Matrice->Bool
corect (M []) = True
corect (M (L x : [])) = True
corect (M (L x1 : L x2 : xs2)) = length x1 == length x2 && corect (M (L x2 : xs2))