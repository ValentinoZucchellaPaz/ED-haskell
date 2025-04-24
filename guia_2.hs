-- evaluacion de funciones y tipado
-- 2
promedio :: Int -> Int -> Float
promedio a b = fromIntegral (a + b) / 2

-- funciones por casos
-- 7 defini una funcion que retorne el signo de un entero (0 neg 1 pos, -1 otro)
signo :: Int -> Int
signo x
  | x > 0 = 1
  | x < 0 = 0
  | otherwise = -1

-- 8
entre0y9 :: Int -> Bool
entre0y9 x
  | x >= 0 && x <= 9 = True
  | otherwise = False

-- 9 Defini la funcion rangoPrecio :: Int -> String, dado un numero retorne “muy barato” si el precio es menor a 2000, “demasiado caro” mayor que 5000, “hay que verlo bien” entre 2000 y 5000, y “esto no puede ser!” negativo.
rangoPrecio :: Int -> String
rangoPrecio x
  | x < 0 = "no puede ser"
  | x < 2000 = "muy barato"
  | x <= 5000 = "hay que ver"
  | otherwise = "muy barato"

-- 10
absoluto :: Int -> Int
absoluto = abs -- aca usa Eta Reduce (creo que asigna la funcion abs a la nueva funcion en vez de hacer un pasamanos de parametros)

-- 11
esMultiplo2 :: Int -> Int -> Bool
esMultiplo2 x y
  | mod x y == 0 = True
  | otherwise = False

-- 12 funcion que dadas las componentes de un vector devuelve el cuadrante en el q esta
cuadrante :: Float -> Float -> Float
cuadrante x y
  | x > 0 && y > 0 = 1
  | x > 0 && y < 0 = 4
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | otherwise = 0

-- composicion de funciones
-- 13 
esMultiploDe:: Int -> Int -> Bool
esMultiploDe dividendo divisor  = mod dividendo divisor == 0

-- 14 
esBisiesto:: Int -> Bool
esBisiesto x | mod x 400 == 0 = True
            | mod x 4 == 0 && mod x 100 /= 0 = True
            | otherwise = False

-- 15 dispersion toma 3 float y devuelve la resta entre el valor mas alto y mas bajo, se usan funciones auxiliares para sacar el mas alto y mas bajo de 3 valores
max3:: Float -> Float -> Float -> Float
max3 x y z = max (max x y) z
min3:: Float -> Float -> Float -> Float
min3 x y z = min (min x y) z
dispersion :: Float -> Float -> Float -> Float
dispersion x y z = max3 x y z - min3 x y z

-- 16
tresDiferentes :: Int -> Int -> Int -> Bool
tresDiferentes x y z | x == y && x == z = True | otherwise = False

-- 17 
celcToFar :: Float -> Float
celcToFar x = x * 1.8 + 32

-- 18
farToCelc :: Float -> Float
farToCelc x = (x-32) / 1.8

-- 19 
haceFrio :: Float -> Bool
haceFrio f | farToCelc f < 8 = True | otherwise = False

-- 20
volEsfera :: Float -> Float
volEsfera radius = (4/3) * pi * radius**3

-- 21 a -> 10$, b -> 20$, c->50$, d->100$
sumaBilletes :: Int -> Int -> Int -> Int -> Int
sumaBilletes a b c d = a*10 + b*20 + c*50 + d*100

-- funciones recursivas
-- 22 sumatoria desde 1 hasta n
sumatoria :: Int -> Int
sumatoria 1 = 1
sumatoria n = n + sumatoria(n-1)

-- 23
potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x n = x * potencia x (n-1)

-- 24
sumaPares :: Int -> Int
sumaPares 1 = 2
sumaPares n = 2 * n + sumaPares (n-1)

-- 25 algoritmo de euclides (calcula max comun divisor)
euclides :: Int -> Int -> Int
euclides a 0 = a
euclides a b | b > 0 = euclides b (a `mod` b) | otherwise = 0

-- 26 prod impares 
prodImpares :: Int -> Int
prodImpares 1 = 3
prodImpares n = (2*n + 1) * prodImpares (n-1)

-- 27 doble factorial n!! = n * (n-2) * (n-4) * ... * 3 * 1 (impar)
-- n!! = n * (n-2) * (n-4) * ... * 4 * 2 (par)
dobleFactorial :: Int -> Int
dobleFactorial 2 = 2
dobleFactorial 3 = 3
dobleFactorial n = n * dobleFactorial(n-2)
