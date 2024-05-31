import Cp
import List
import ListUtils
--import LTree

-- P1 --------------------------------------------

data Party = A|B|C|D deriving (Eq, Ord, Show)
vote = [(A, 12000),(B, 7500),(C, 4500),(D, 3000)]
db = map f vote where f(a,b) = (a,(b,0))
--history = [for (step db) i | i <- [0..7]]
final = map (id >< p2) . last
total = sum (map p2 vote)

auxP :: ((a,(Integer,Integer)),(a,(Integer,Integer))) -> (a,(Integer,Integer))
auxP ((p,(v,c)),(_,(0,_))) = (p,(v,c))
auxP ((_,(0,_)),(p,(v,c))) = (p,(v,c))
auxP ((p1,(v1,c1)),(p2,(v2,c2))) | (div v1 (c1+1)) > (div v2 (c2+1)) = (p1,(v1,c1))
                                 | (div v1 (c1+1)) < (div v2 (c2+1)) = (p2,(v2,c2))
                                 | otherwise = if (v2<v1) then (p2,(v2,c2))
                                                          else (p1,(v1,c1))

find = cataList aux
aux = either (const (D,(0,0))) auxP

--update = cataList ud
--ud = either nil (cons . (id >< (id >< succ)))
update :: ([(Party,(Integer, Integer))], (Party,(Integer, Integer))) -> [(Party,(Integer, Integer))]
update ([], _) = []
update (l, (_,(0,0))) = l
update ((h:t), a) | h == a = let udAux (a,(b,c)) = (a,(b,c+1)) in udAux h : t
                  | otherwise = (h : (update (t, a)))

step :: [(Party,(Integer, Integer))] -> Int -> [(Party,(Integer, Integer))]
step l 0 = l
step l _ = update (l, find l) -- l = last history

--wasted = waste history
--waste = sum . (map (ddiv.p2)) . last
--ddiv (a,b) = div a b

-- P1 end ----------------------------------------

-- P2 --------------------------------------------
mergeC = cataList (f)
f = either nil merge

mergek = hyloList f outList

mSortk :: Integer -> [Integer] -> [Integer]
mSortk k l= mergek (splitk k l)

splitk :: Integer -> [Integer] -> [[Integer]]
splitk _ [] = []
splitk 1 l = [l]
splitk n (h:t)= [h] : (splitk (n-1) t)

merge :: ([Integer],[Integer]) -> [Integer]
merge (l,[])                  = l
merge ([],r)                  = r
merge (x:xs,y:ys) | x < y     = x : merge(xs,y:ys) 
                  | otherwise = y : merge(x:xs,ys)
--------------------------------------------------
minL = cataList g
g = either (const 100) minA where minA (a,b) = min a b

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:y:z) | y<x = sort(y : sort (x:z))
             | otherwise = x:sort (y:z)
-- P2 end ----------------------------------------

-- P3 --------------------------------------------
---
-- P3 end ----------------------------------------
-- P4 --------------------------------------------
len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len t
--------------------------------------------------
maxL = cataList h
h = either zero maxA where maxA (a,b) = max a b
-- P4 end ----------------------------------------