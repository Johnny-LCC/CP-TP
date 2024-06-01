import Cp
import List ( hyloList, outList )
import LTree ( mSort )

f = either nil (mSort . conc)

mergek = hyloList f outList

mSortk :: Integer -> [Integer] -> [Integer]
mSortk k l = mergek (splitk k l)

splitk :: Integer -> [b] -> [[b]]
splitk _ [] = []
splitk 1 l = [l]
splitk n (h:t)= [h] : (splitk (n-1) t)
