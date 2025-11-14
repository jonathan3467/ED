module Aux (Arbol(..),contarFrecuencias, frecuencias, quitarRepetidos, pertenece,contar, ordenafrecuencia, insertar, construir, construirizq,codigosAux, decodAux) where

data Arbol a = Vacio | AB a (Arbol a) (Arbol a) deriving (Eq, Ord, Show)

{-
Estos seran auxiliares para una funcion extra aunque no muy necesaria, pero es
para que quede claro como queda la lista y cuantas veces se repite cada una
-}
-- recorre las letras sin repeteir y cuenta cuantas veces aparece cada una
contarFrecuencias :: String -> String -> [(Char, Int)]
contarFrecuencias [] _ = []
contarFrecuencias (x:xs) palabra = (x, contar x palabra) : contarFrecuencias xs palabra

--le pasas una frase o palabra y regresa una lista con cada eltra y cuentas beves aparece

frecuencias :: String -> [(Char, Int)]
frecuencias [] = []
frecuencias palabra = contarFrecuencias (quitarRepetidos palabra) palabra

{-
Estas son funciones auxiliares para en si a primera funcion que nos ayuda a
todos los demas codigos 
-}

-- quita las letas repetidas
quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs)
  |pertenece x xs = quitarRepetidos xs
  |otherwise = x : quitarRepetidos xs

-- Verifica si un elemento pertenece a una lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
  | e == x = True
  | otherwise = pertenece e xs

-- cuenta cuantas veces aparece una letra
contar :: Eq a => a -> [a] -> Int
contar _ [] = 0
contar c (x:xs)
  |c == x = 1+contar c xs
  | otherwise = contar c xs

-- Ordena las letras segun su frecuencia
ordenafrecuencia :: String -> String -> [Char]
ordenafrecuencia [] _ = []
ordenafrecuencia (x:xs) palabra =
  insertar x (ordenafrecuencia xs palabra) palabra

-- Inserta la letra en una lista de mayor a menor frecuencia
insertar :: Char -> [Char] -> String -> [Char]
insertar c [] palabra = [c]
insertar c (x:xs) palabra
  | contar c palabra >= contar x palabra = c : x : xs
  | otherwise = x : insertar c xs palabra

-- Todo lo anterior es para lo primero que es darle una palabra y que te
--regrese la lista de mayor a menor frecuencia dependiendo la letra

{- Este es para en el lado derecho poner el elemento de la lista, donde la raiz la llamamos literalmente raiz de ahi se crean dos subarboles, donde en el derecho se deja vacio y se coloca el primer elemento de la lista -}

construir :: [Char] -> Arbol Char
construir [] = Vacio
construir [x] = AB x Vacio Vacio
construir (x:xs) = AB 'R' (construirizq xs) (AB x Vacio Vacio)

-- esta es para ir construyendo los lados izquierdos del arbol que son 0, mientras que en el subarboles derecho van dejando el elemento que les sobro
construirizq :: [Char] -> Arbol Char
construirizq [] = Vacio
construirizq [y] = AB '0' Vacio (AB y Vacio Vacio)
construirizq (y:ys) = AB '0' (construirizq ys) (AB y Vacio Vacio)

{-
Este es el auxiliar para la tercera funcion
-}

-- auxiliar que construye los códigos binarios recursivamente
codigosAux :: Arbol Char -> String -> [(Char, String)]
codigosAux Vacio _ = []
codigosAux (AB c izq der) codigo
  | c == 'R' = codigosAux izq (codigo ++ "0") ++ codigosAux der (codigo ++ "1")
  | c == '0' = codigosAux izq (codigo ++ "0") ++ codigosAux der (codigo ++ "1")
  | otherwise = [(c, codigo)] ++ codigosAux izq (codigo ++ "0") ++ codigosAux der (codigo ++ "1")

{-
Esta va a ser el uxiliar para la cuarta funcion y ultima, que lo que hace es
recorrer el arbol segun los bits que le demos, donde si llegamos a una hoja la agregamos y regresamos a la raiz, si el bit es 0 vamos a la izquierda, si es 1 vamos a la derecha
-}

decodAux :: Arbol Char -> String -> Arbol Char -> String
-- si ya no hay bits pero estamos en una hoja devolvemos esa letra
decodAux (AB r izq der) [] _
  | r /= 'R' && r /= '0' && izq == Vacio && der == Vacio = [r]
decodAux _ [] _ = []
decodAux (AB r izq der) (b:bs) raiz
-- si llegamos a una letra la agregamos y regresamos a la raiz
  | r /= 'R' && r /= '0' && izq == Vacio && der == Vacio = r : decodAux raiz (b:bs) raiz
  -- si el bit es 0 ahora pa la izquierda
  | b == '0' = decodAux izq bs raiz
  -- si el bit es 1 vamos pa la derecha
  | b == '1' = decodAux der bs raiz
  -- en otro caso pues vacio
  | otherwise = []