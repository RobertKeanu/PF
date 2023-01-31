import Data.List (nub)
import Data.Maybe (fromJust)

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
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not( Var "P") :&:Not( Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: (Not(Var "P") :|:Not( Var "Q") :&: Not(Var "P") :|:Not( Var "R"))

instance Show Prop where
    show (Var nume) = nume
    show F = "F"
    show T = "T"
    show (Not prop) = "(" ++ "~" ++ show prop ++ ")"
    show (prop :|: prop2) = "(" ++ show prop ++ "|" ++ show prop2 ++ ")"
    show (prop :&: prop2) = "(" ++ show prop ++ "&" ++ show prop2 ++ ")"
    show (prop :->: prop2) = "(" ++ show prop ++ " -> " ++ show prop2 ++ ")"
    show (prop :<->: prop2) = "(" ++ show prop ++ " <-> " ++ show prop2 ++ ")"

type Env = [(Nume, Bool)]

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

variabile :: Prop -> [Nume]
variabile (Var a) = [a]
variabile F = []
variabile T = []
variabile (Not a) = variabile a 
variabile (a :|: b) = nub(variabile a ++ variabile b)
variabile (a :&: b) = nub(variabile a ++ variabile b)
variabile (a :->: b) = nub(variabile a ++ variabile b)
variabile (a :<->: b) = nub (variabile a ++ variabile b)

envs :: [Nume] -> [Env]
envs l = [zip l e | e <- sequence (take (length l) (repeat [False, True]))]

satisfiabila :: Prop -> Bool
satisfiabila p = any (eval p) (envs (variabile p))

valida :: Prop -> Bool
valida p = all (eval p) (envs (variabile p))

echivalenta :: Prop -> Prop -> Bool
echivalenta prop1 prop2 = valida (prop1 :<->: prop2)

