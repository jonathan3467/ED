module Aux (Arbol(..), quitarRepetidos, pertenece,contar, ordenafrecuencia, insertar, construir, construirizq) where

data Arbol a = Vacio | AB a (Arbol a) (Arbol a) deriving (Eq, Ord, Show)

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

frecuenciaLetras :: String -> [Char]
frecuenciaLetras [] = []
frecuenciaLetras palabra = ordenafrecuencia (quitarRepetidos palabra) palabra

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