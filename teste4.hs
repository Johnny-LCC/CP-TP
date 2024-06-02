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

lista :: [[Integer]]
lista = [ld x | x <- l]

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

-- sum(teste lista) = 10

------------------------------------------------
-- eu acho que é preciso transformar tudo isto
-- em apnas um hilomorfismo, do tipo:

--k = hyloList f g
-- g (ana) :: [a] -> [[a]]
-- f (cata) :: [[a]] -> a
-------------------------------------------------
