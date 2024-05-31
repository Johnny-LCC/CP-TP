import Cp
import List

mergeC = cataList (f)
f = either nil merge

mergek = hyloList f outList

mSortk :: Integer -> [Integer] -> [Integer]
mSortk k l= mergek (splitk k l)

splitk :: Integer -> [b] -> [[b]]
splitk _ [] = []
splitk 1 l = [l]
splitk n (h:t)= [h] : (splitk (n-1) t)

merge :: Ord a => ([a],[a]) -> [a]
merge (l,[])                  = l
merge ([],r)                  = r
merge (x:xs,y:ys) | x < y     = x : merge(xs,y:ys) 
                  | otherwise = y : merge(x:xs,ys)

------------------- CONCLUIDO --------------------