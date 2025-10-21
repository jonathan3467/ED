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
