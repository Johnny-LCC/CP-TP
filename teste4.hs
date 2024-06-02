import Cp
import List ( cataList, anaList )
import Nat ( outNat )

l :: [Integer]
l = [2,1,5,6,2,3]

len :: [a] -> Integer
len = foldr (\ h -> (+) 1) 0

ld :: Integer -> [Integer]
ld = anaList ((id -|- split succ id) . outNat)
-- ld 5 = [5,4,3,2,1] -- ld 3 = [3,2,1] -- ld 0 = []

lista :: [Integer] -> [[Integer]]
lista l = [ld x | x <- l]

teste :: [[Integer]] -> [Integer]
teste = cataList (either nil maxR)
-- teste lista = [5,5]

maxR :: ([Integer], [Integer]) -> [Integer]
maxR (h:t, []) = [h] 
maxR (h:t, l)| head l >= h = if sum l > ((len l)+1) * h then l else h : sub h l 
             | otherwise = if h > sum l then [h] else head l: l

sub :: Integer -> [Integer] -> [Integer]
sub _ [] = []
sub n (h:t) = n : sub n t

lrh = sum . teste . lista

------------------HILOMORFISMO---------------------
lrh' = sum . hyloList f g
f = either nil maxR
g = (id -|- (cons . split succ singl >< id)) . out

out :: [Integer] -> Either () (Integer, [Integer])
out [] = i1 ()
out (h:t) = i2 (h-1,t)

-- Acho que o anamorfismo (g) não está perfeito,
-- talvez precise de alterações! 
---------------------------------------------------
