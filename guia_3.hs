-- ejercicios sobre listas: recordar que haskell es un lenguaje funcional que no tiene variables, por lo q debo hacer pasamanos entre funciones para lograr un resultado
-- tuplas 1 a-f (vectores)
-- 1a
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
segundo3 :: (Int, Int, Int) -> Int
segundo3 (a, b, c) = b

-- 1b
ordena :: (Int, Int) -> (Int, Int)
ordena (a, b) | a > b = (a, b) | otherwise = (b, a)

-- 1c :: se supone que la tupla de rango de precios siempre va a ser menor, mayor
rangoPrecioParametizado :: Int -> (Int, Int) -> String
rangoPrecioParametizado x (menor, mayor)
  | x < 0 = "Esto no puede ser"
  | x < menor = "Muy barato"
  | x > mayor = "Demasiado caro"
  | otherwise = "Hay que verlo bien"

-- 1d
mayorque3 :: (Int, Int, Int) -> (Bool, Bool, Bool)
mayorque3 (a, b, c) = (a > 3, b > 3, c > 3)

-- 1e
todosiguales :: (Int, Int, Int) -> Bool
todosiguales (a, b, c) | a == b && a == c = True | otherwise = False

-- 1f
condicionfinal :: (Num a, Ord a) => (String, a, a, a) -> (String, String)
condicionfinal (nombre, nota_p_1, nota_p_2, nota_p_3)
  | nota_p_1 > 7 && nota_p_2 > 7 && nota_p_3 > 7 = (nombre, "Promoción")
  | nota_p_1 > 4 && nota_p_2 > 4 && nota_p_3 > 4 = (nombre, "Regular")
  | otherwise = (nombre, "Libre")

-- funciones recursivas 5-14
-- -------------------------------------------------------------------------------------------------------
-- 5 funciones tipo FILTRO
-- a funciones de tipo filtro: aquella que dada una lista devuelve otra lista cuyos elementos son los elementos de la primera, en el mismo orden, que cumplan una determinada condición.
solopares :: [Int] -> [Int]
solopares [] = []
solopares (x : xs)
  | even x = x : solopares xs
  | otherwise = solopares xs

-- b
mayorque10 :: [Int] -> [Int]
mayorque10 [] = []
mayorque10 (x : xs)
  | x > 10 = x : mayorque10 xs
  | otherwise = mayorque10 xs

-- c
mayorque :: Int -> [Int] -> [Int]
mayorque _ [] = []
mayorque n (x : xs)
  | x > n = x : mayorque n xs
  | otherwise = mayorque n xs

-- --------------------------------------------------------------------------------------------
-- 6 funciones de tipo MAPEO: aquella que dada una lista devuelve otra lista cuyos elementos son los que se obtienen de aplicar una función que transforma cada elemento.
-- a
sumar1 :: [Int] -> [Int]
sumar1 = map (+ 1)

-- version sin etaReduce
-- sumar1 xs = map (+ 1) xs
-- version sin map ni etaReduce
-- sumar1 [] = []
-- sumar1 (x:xs) = (x+1) : sumar1 xs

-- b
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x : xs) = (x * 2) : duplica xs

-- c
multiplica :: Int -> [Int] -> [Int]
multiplica _ [] = []
multiplica n (x : xs) = (x * n) : multiplica n xs

-- ----------------------------------------------------------------------------------------
-- 7 funciones de tipo ACUMULADOR:  aquella que dada una lista devuelve un valor resultante de combinar los elementos de la lista
-- a
todosmenoresque10 :: [Int] -> Bool
todosmenoresque10 = foldr (\x -> (&&) (x < 10)) True

-- version sin etaReduce
-- todosmenoresque10 xs = foldr (\ x -> (&&) (x < 10)) True xs
-- version sin foldr ni etaReduce
-- todosmenoresque10 [] = True
-- todosmenoresque10 (x:xs) = x<10 && todosmenoresque10 xs
-- b
hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x : xs) = x == 0 || hay0 xs

-- c
prodcuadrados :: [Int] -> Int
prodcuadrados [] = 1
prodcuadrados (x : xs) = x * x + prodcuadrados xs

-- ----------------------------------------------------------------------------------------------
-- 8 definir funciones y determinar si son tipo filtro, mapeo o acumulador
-- a acc
maximo :: [Int] -> Int
maximo [] = 0
maximo (x : xs)
  | x > maximo xs = x
  | otherwise = maximo xs

-- b acc
sumaPares :: [(Int, Int)] -> Int
sumaPares [] = 0
sumaPares ((x, y) : xs) = x + y + sumaPares xs

-- c acc
todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x : xs)
  | x == 0 || x == 1 = todos0y1 xs
  | otherwise = False

-- d filtro
quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x : xs)
  | x == 0 = quitar0s xs
  | otherwise = x : quitar0s xs

-- e acc
ultimo :: [a] -> a
ultimo (x : xs)
  | null xs = x
  | otherwise = ultimo xs

-- f mapeo
repetir :: Int -> Int -> [Int]
repetir 0 _ = []
repetir n x = x : repetir (n - 1) x

-- g mapeo
concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x : xs) = x ++ concatenar xs

-- h mapeo
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- --------------------------------------------------------------------
-- 9 explicar que hace la funcion, no como
g :: [[Int]] -> [Int]
g [] = []
g (x : xs) = length x : g xs

-- la funcion acepta una lista de listas y retorna la longitud de cada lista interior en su debida posicion de la lista "madre"

-- ----------------------------------------------------------------------
-- 10 zip
repartir :: [String] -> [String] -> [(String, String)]
repartir [] _ = []
repartir (p : personas) (c : cartas) = (p, c) : repartir personas cartas

-- ----------------------------------------------------------------------
-- 11 unzip
apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((_, apellido, _) : personas) = apellido : apellidos personas

-- ----------------------------------------------------------------------
-- 12 length, !!, head, tail, take, drop, ++
length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

indexacion :: [a] -> Int -> a
indexacion (x : xs) 0 = x
indexacion (x : xs) n = indexacion xs (n - 1)

head' :: [a] -> a
head' [] = error "no se puede obtener el primer elemento de una lista vacia"
head' (x : xs) = x

tail' :: [a] -> [a]
tail' [] = error "no se puede obtener el ultimo elemento de una lista vacia"
tail' (x : xs) = xs

take' :: Integer -> [a] -> [a]
take' _ [] = []
take' n (x : xs) | n <= 0 = [] | otherwise = x : take' (n - 1) xs

drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x : xs)
  | n < 0 = xs
  | otherwise = drop' (n - 1) xs

concatenar' :: [a] -> [a] -> [a]
concatenar' [] y = y
concatenar' (x : xs) y = x : concatenar' xs y

-- ----------------------------------------------------------------------
-- 13 Defini por recursion las siguientes funciones e identifica si pertenecen a alguna de las categorias filtro, mapeo o acumulador. a,b,c,d
-- a acc
listasIguales :: forall a. (Eq a) => [a] -> [a] -> Bool
listasIguales [] [] = True
listasIguales (x : xs) (y : ys) = x == y && listasIguales xs ys

-- b filtro
mejorNota :: [(String, Int, Int, Int)] -> [(String, Int)]
mejorNota [] = []
mejorNota ((nombre, n1, n2, n3) : xs) = (nombre, max (max n1 n2) n3) : mejorNota xs

-- c mapeo
incPrimero :: [(Int, Int)] -> [(Int, Int)]
incPrimero [] = []
incPrimero ((a, b) : xs) = (a + 1, b) : incPrimero xs

-- d mapeo
expandir :: String -> String
expandir "" = ""
expandir (c : s) = c : ' ' : expandir s

-- ----------------------------------------------------------------------
-- 14 escribir funciones para filtrado de pelis para una app de pelis a,b,c,d
-- (⟨Nombre de la pel´ıcula⟩,⟨A˜no de estreno⟩,⟨Duraci´on de la pel´ıcula⟩,⟨Nombre del director⟩)
verTodas :: [(String, Int, Int, String)] -> Int
verTodas [] = 0
verTodas ((_, _, d, _) : peliculas) = d + verTodas peliculas

estrenos :: [(String, Int, Int, String)] -> [String]
estrenos [] = []
estrenos ((n, a, _, _) : peliculas) | a == 2025 = n : estrenos peliculas | otherwise = estrenos peliculas

filmografia :: [(String, Int, Int, String)] -> String -> [String]
filmografia [] _ = []
filmografia ((n, _, _, dir) : peliculas) director
  | director == dir = n : filmografia peliculas director
  | otherwise = filmografia peliculas director

duracion :: [(String, Int, Int, String)] -> String -> Int
duracion [] nombre = error ("no se encontró la pelicula llamada " ++ nombre)
duracion ((n, _, d, _) : peliculas) nombre
  | nombre == n = d
  | otherwise = duracion peliculas nombre
