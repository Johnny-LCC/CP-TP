import Cp
import Nat ( for )
import List ( cataList )

data Party = A|B|C|D deriving (Eq, Ord, Show)
type Votes = Integer
type Deputies = Integer

vote = [(A, 12000),(B, 7500),(C, 4500),(D, 3000)]
db = map f vote where f(a,b) = (a,(b,0))
history = [for step db i | i <- [0..7]]
result = final history 
final = map (id >< p2) . last
total = sum (map p2 vote)
wasted = waste history

waste = sum . (map (uncurry div.(id >< succ).p2)) . last

step = update . (split id find)

find = cataList (either (const (D,(0,0))) aux)

aux :: ((a,(Votes, Deputies)),(a,(Votes, Deputies))) -> (a,(Votes, Deputies))
aux ((p,(v,c)),(_,(0,_))) = (p,(v,c))
aux ((_,(0,_)),(p,(v,c))) = (p,(v,c))
aux ((p1,(v1,c1)),(p2,(v2,c2))) | (div v1 (c1+1)) > (div v2 (c2+1)) = (p1,(v1,c1))
                                 | (div v1 (c1+1)) < (div v2 (c2+1)) = (p2,(v2,c2))
                                 | otherwise = if (v2<v1) then (p2,(v2,c2))
                                                          else (p1,(v1,c1))

update :: Eq b => ([(b,(Votes, Deputies))], (b,(Votes, Deputies))) -> [(b,(Votes, Deputies))]
update ([], _) = []
update (l, (_,(0,0))) = l
update ((h:t), a) | h == a = let udAux (a,(b,c)) = (a,(b,c+1)) in udAux h : t
                  | otherwise = (h : (update (t, a)))
