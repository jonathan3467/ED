import Aux
frecuenciaLetras :: String -> [Char]
frecuenciaLetras [] = []
frecuenciaLetras palabra = ordenafrecuencia (quitarRepetidos palabra) palabra

construirHuffman :: [Char] -> Arbol Char
construirHuffman [] = Vacio
construirHuffman xs = construir xs

codigos :: Arbol Char -> [(Char, String)]
codigos Vacio = []
codigos (AB c izq der) 
  | c == 'R' = codigosAux izq "0" ++ codigosAux der "1"
  | otherwise = codigosAux (AB c izq der) ""