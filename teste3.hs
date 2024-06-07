-- Funções mutuamente recursivas --
f :: Integer -> Integer
f 0 = 1
f n = g n 0

g :: Integer -> Integer -> Integer
g n k | n == k = 0
      | otherwise = f k * f (n-1-k) + g n (k+1)

-- para calcular o número de Catalan n (f n) --

cat = prj . for loop inic where
    loop (a,b) = (b,a*b) -- não está correto
    inic = (1,1)
    prj = id
