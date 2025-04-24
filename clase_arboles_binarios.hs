-- Arboles binarios
-- ejercicio clase: hacer un arbol de caracteres y funcion que se le pase el arbol y el caracter a buscar y diga si esta en el arbol
data ArbolChar = Hoja | Nodo Char ArbolChar ArbolChar
  deriving (Show)

arbolChar1 =
  Nodo
    'A'
    ( Nodo
        'B'
        (Nodo 'D' Hoja Hoja)
        (Nodo 'E' Hoja Hoja)
    )
    ( Nodo
        'C'
        (Nodo 'F' Hoja Hoja)
        Hoja
    )

contieneNodo :: ArbolChar -> Char -> Bool
contieneNodo Hoja _ = False
contieneNodo (Nodo x hi hd) charBuscado
  | charBuscado == x = True
  | otherwise = contieneNodo hi charBuscado || contieneNodo hd charBuscado

-- arbol bin de cualquier tipo
data ArbolBin a = HojaBin | NodoBin a (ArbolBin a) (ArbolBin a)
  deriving (Show)

arbolEjemplo =
  NodoBin
    4
    (NodoBin 2 (NodoBin 1 HojaBin HojaBin) (NodoBin 3 HojaBin HojaBin))
    (NodoBin 7 (NodoBin 5 HojaBin (NodoBin 6 HojaBin HojaBin)) (NodoBin 8 HojaBin HojaBin))

-- orden: pre, in, post
preorder :: ArbolBin a -> [a]
preorder HojaBin = []
preorder (NodoBin x ai ad) = x : preorder ai ++ preorder ad

inorder :: ArbolBin a -> [a]
inorder HojaBin = []
inorder (NodoBin x ai ad) = inorder ai ++ [x] ++ inorder ad

postorder :: ArbolBin a -> [a]
postorder HojaBin = []
postorder (NodoBin x ai ad) = postorder ai ++ postorder ad ++ [x]
