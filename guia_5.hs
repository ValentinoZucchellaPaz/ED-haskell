-- Arboles binarios
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
data ArbolInt = Hoja | Nodo Int ArbolInt ArbolInt
  deriving (Show)

-- 1
arbolito1 =
  Nodo
    1
    ( Nodo
        2
        Hoja
        ( Nodo 4 Hoja Hoja
        )
    )
    ( Nodo
        3
        Hoja
        ( Nodo 5 Hoja Hoja
        )
    )

-- 2
tamaño :: ArbolInt -> Int
tamaño Hoja = 0
tamaño (Nodo _ hi hd) = 1 + tamaño hi + tamaño hd

-- 3
altura :: ArbolInt -> Int
altura Hoja = 0
altura (Nodo _ hi hd) = 1 + max (altura hi) (altura hd)

-- 4
sumaNodo :: ArbolInt -> Int
sumaNodo Hoja = 0
sumaNodo (Nodo x hd hi) = x + sumaNodo hd + sumaNodo hi

-- 5
data ArbolChar = HojaChar | NodoChar Char ArbolChar ArbolChar
  deriving (Show)

-- 6
arbolChar1 =
  NodoChar
    'A'
    ( NodoChar
        'B'
        (NodoChar 'D' HojaChar HojaChar)
        (NodoChar 'E' HojaChar HojaChar)
    )
    ( NodoChar
        'C'
        (NodoChar 'F' HojaChar HojaChar)
        HojaChar
    )

-- 7
ocurre :: Char -> ArbolChar -> Bool
ocurre _ HojaChar = False
ocurre charBuscado (NodoChar x hi hd)
  | charBuscado == x = True
  | otherwise = ocurre charBuscado hi || ocurre charBuscado hd

-- 8
arbolChar2String :: ArbolChar -> String
arbolChar2String HojaChar = ""
arbolChar2String (NodoChar charsito ai ad) = charsito : arbolChar2String ai ++ arbolChar2String ad

-- 9
arbolChar2String' :: ArbolChar -> String
arbolChar2String' HojaChar = ""
arbolChar2String' (NodoChar charsito ai ad) = (arbolChar2String' ai ++ arbolChar2String' ad) ++ [charsito]

-- arboles polimorficos
-- 10
data ArbPolimorf a = HojaPol | NodoPol a (ArbPolimorf a) (ArbPolimorf a)

tamanoPol :: ArbPolimorf a -> Int
tamanoPol HojaPol = 0
tamanoPol (NodoPol _ ai ad) = 1 + tamanoPol ai + tamanoPol ad

alturaPol :: ArbPolimorf a -> Int
alturaPol HojaPol = 0
alturaPol (NodoPol _ ai ad) = 1 + max (alturaPol ai) (alturaPol ad)

-- 11
preorder :: ArbPolimorf a -> [a]
preorder HojaPol = []
preorder (NodoPol x ai ad) = x : preorder ai ++ preorder ad

inorder :: ArbPolimorf a -> [a]
inorder HojaPol = []
inorder (NodoPol x ai ad) = inorder ai ++ [x] ++ inorder ad

postorder :: ArbPolimorf a -> [a]
postorder HojaPol = []
postorder (NodoPol x ai ad) = postorder ai ++ postorder ad ++ [x]

-- 12
balanceado :: ArbPolimorf a -> Bool
balanceado HojaPol = True
balanceado (NodoPol x ai ad)
  | alturaPol ai == alturaPol ad = balanceado ai && balanceado ad
  | otherwise = False

-- 13 toma una arbol de pares de enteros (Int, Int) y devuelve uno de enteros con el valor max del par
pairwiseMax :: ArbPolimorf (Int, Int) -> ArbPolimorf Int
pairwiseMax HojaPol = HojaPol
pairwiseMax (NodoPol (a, b) ai ad) = NodoPol (max a b) (pairwiseMax ai) (pairwiseMax ad)

-- 14 corta las ramas cuyo valor supere el dado
poda :: Int -> ArbPolimorf Int -> ArbPolimorf Int
poda _ HojaPol = HojaPol
poda a (NodoPol x ai ad)
  | x > a = HojaPol
  | otherwise = NodoPol x (poda a ai) (poda a ad)

-- ver arboles con valor en las hojas y y arbol con mas de un subarbol
-- arboles con valores en las hojas
-- 15
data ArbolH a = NodoH (ArbolH a) (ArbolH a) | HojaH a
  deriving (Show)

-- 16
arbol16 :: ArbolH Bool
arbol16 =
  NodoH
    ( NodoH
        ( NodoH
            (HojaH True)
            (HojaH False)
        )
        ( NodoH
            (HojaH False)
            ( NodoH
                (HojaH True)
                (HojaH False)
            )
        )
    )
    (HojaH True)

arbolIzq16 =
  NodoH
    ( NodoH
        (HojaH True)
        (HojaH False)
    )
    ( NodoH
        (HojaH False)
        ( NodoH
            (HojaH True)
            (HojaH False)
        )
    )

-- 17
hojas :: ArbolH a -> Int
hojas (HojaH _) = 1
hojas (NodoH ai ad) = hojas ai + hojas ad

-- 18 si p c nodo interno la cant de hojas de c subarbol difiere como máximo en 1
balanceado' :: ArbolH a -> Bool
balanceado' (HojaH _) = True
balanceado' (NodoH ai ad)
  | abs (hojas ai - hojas ad) <= 1 = balanceado' ad && balanceado' ai
  | otherwise = False

-- 19 devuelve un array con los valores de las hojas de izq a der
fringe :: ArbolH a -> [a]
fringe (HojaH x) = [x]
fringe (NodoH ai ad) = fringe ai ++ fringe ad

-- 20
listaOrdenada :: [Integer] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (x : y : resto) = x <= y && listaOrdenada (y : resto)

ordenado :: ArbolH Integer -> Bool
ordenado arbol = listaOrdenada (fringe arbol)

arbolintord :: ArbolH Integer
arbolintord = NodoH (NodoH (HojaH (-1)) (HojaH 2)) (NodoH (HojaH 2) (HojaH 3))

arbolintnoord :: ArbolH Integer
arbolintnoord = NodoH (NodoH (HojaH (-1)) (HojaH 2)) (NodoH (HojaH 3) (HojaH 2))

-- 21 -> 23 arboles con numero variable de hijos
-- 21
data ArbolVar a = NodoVar a [ArbolVar a]
  deriving (Show, Eq)

-- 22
arbolVarInteger :: ArbolVar Integer
arbolVarInteger =
  NodoVar
    1
    [ NodoVar
        2
        [ NodoVar 5 [],
          NodoVar 6 []
        ],
      NodoVar 3 [],
      NodoVar
        4
        [ NodoVar 7 [],
          NodoVar 8 [],
          NodoVar 9 [],
          NodoVar 10 []
        ]
    ]

-- 23 grado es la cant de hijos de un nodo
-- gradoMax compara la cant de nodos suyos, contra el máximo grado de sus hijos
-- gradoMaxLista compara el máximo grado del primer arbol contra el de sus hermanos

gradoMax :: ArbolVar a -> Int
gradoMax (NodoVar _ []) = 0
gradoMax (NodoVar _ hijos) = max (length hijos) (gradoMaxLista hijos)

gradoMaxLista :: [ArbolVar a] -> Int
gradoMaxLista [] = 0
gradoMaxLista (x : xs) = max (gradoMax x) (gradoMaxLista xs)
