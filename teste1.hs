import Cp
import Nat
import List

data Party = A|B|C|D deriving (Eq, Ord, Show)

vote = [(A, 12000),(B, 7500),(C, 4500),(D, 3000)]
db = map f vote where f(a,b) = (a,(b,0))
history = [for step db i | i <- [0..7]]
result = final history 
final = map (id >< p2) . last
total = sum (map p2 vote)

auxP :: ((a,(Integer,Integer)),(a,(Integer,Integer))) -> (a,(Integer,Integer))
auxP ((p,(v,c)),(_,(0,_))) = (p,(v,c))
auxP ((_,(0,_)),(p,(v,c))) = (p,(v,c))
auxP ((p1,(v1,c1)),(p2,(v2,c2))) | (div v1 (c1+1)) > (div v2 (c2+1)) = (p1,(v1,c1))
                                 | (div v1 (c1+1)) < (div v2 (c2+1)) = (p2,(v2,c2))
                                 | otherwise = if (v2<v1) then (p2,(v2,c2))
                                                          else (p1,(v1,c1))

find = cataList ff
ff = either (const (D,(0,0))) auxP

--update = cataList ud
--ud = either nil (cons . (id >< (id >< succ)))
update :: Eq b => ([(b,(Integer, Integer))], (b,(Integer, Integer))) -> [(b,(Integer, Integer))]
update ([], _) = []
update (l, (_,(0,0))) = l
update ((h:t), a) | h == a = let udAux (a,(b,c)) = (a,(b,c+1)) in udAux h : t
                  | otherwise = (h : (update (t, a)))

step :: [(Party,(Integer, Integer))] -> [(Party,(Integer, Integer))]
step = update . (split id find)

wasted = waste history
waste = sum . (map (ddiv.p2)) . last
ddiv (a,b) = div a (b+1)
