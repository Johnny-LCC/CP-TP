import Cp
import List

len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len t
--------------------------------------------------
maxL = cataList h
h = either zero maxA where maxA (a,b) = max a b