
data Proposition = Const Bool
          | Var Char
          | Not Proposition
          | And Proposition Proposition
          | Imply Proposition Proposition

type Substitution = Assoc Char Bool

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)


eval :: Substitution -> Proposition -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q

vars :: Proposition -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
    where bss = bools (n-1)

substs :: Proposition -> [Substitution]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

isTaut :: Proposition -> Bool
isTaut p = and [eval s p | s <- substs p]


p1 :: Proposition
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Proposition
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Proposition
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B')) 

p4 :: Proposition
p4 = Imply (And (Var 'A') (Imply (Var 'A')(Var 'B'))) (Var 'B')




