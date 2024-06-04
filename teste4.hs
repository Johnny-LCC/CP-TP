import Cp
import List ( cataList, anaList )
import Nat ( outNat )

l :: [Integer]
l = [2,1,5,6,2,3]
--------------------------------------------------------
len :: [a] -> Integer
len = foldr (\ h -> (+) 1) 0

maxR :: ([Integer], [Integer]) -> [Integer]
maxR ([],h:t) = [h] 
maxR (h:t, []) = [h] 
maxR (h:t, l)| head l > h = if sum l > ((len l)+1) * h then singl(sum l) else h : sub h l --
             | head l == h = h:l
             | otherwise = if h >= (sum l + head l) then h:sub 0 l else head l: l 

sub :: Integer -> [Integer] -> [Integer]
sub _ [] = []
sub n (h:t) = n : sub n t

lrh = sum . hyloList f g
f = either nil maxR
g = (id -|- ( singl >< id)) . outList

-- Acho que está concluído, mas preciso de realizar
-- mais alguns testes. Sintam-se livres para fazerem
-- o mesmo
---------------------------------------------------

out :: [Integer] -> Either () (Integer, [Integer])
out [] = i1 ()
out (h:t) = i2 (h-1,t)
