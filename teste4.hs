import Cp
import List ( cataList, anaList , hyloList, outList)

l :: [Integer]
l = [1,3,5,4,3,2,2,3,1,0,3,2,2]
l1 :: [Integer]
l1 = [2,1,5,6,2,3]
l2 :: [Integer]
l2 = [3,5,2,1,5,4]

g :: [Integer] -> [[Integer]]
g = anaList ((id -|- split cons p2) . outList)
f :: [[Integer]] -> [Integer]
f = cataList (either nil ff)
lrh :: [Integer] -> Integer
lrh = maxCata . f . g

mRec :: [Integer] -> Integer
mRec [] = 0 --
mRec (0:t) = 0
mRec (h:t) = max (h * (1 + auxR (h,t))) (mRec(h-1:t))

auxR :: (Integer,[Integer]) -> Integer
auxR (x,[]) = 0
auxR (x,h:t) | x <= h = 1 + auxR(x,t)
             | otherwise = 0

ff :: ([Integer], [Integer]) -> [Integer]
ff = cons.(mRec >< id)

maxCata = cataList (either zero (uncurry max))
-- Vou só dar uma ajeitada nos tipos das funções --
