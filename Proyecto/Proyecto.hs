import Aux
-- Te da una lista con el orden de las letras de mayor a menor frecuencia
frecuenciaLetras :: String -> [Char]
frecuenciaLetras [] = []
frecuenciaLetras palabra = ordenafrecuencia (quitarRepetidos palabra) palabra

-- Construye nuestro arbol conforme la lista dada del anterior codigo
construirHuffman :: [Char] -> Arbol Char
construirHuffman [] = Vacio
construirHuffman xs = construir xs

--Dada el arbol anterior te da el codigo de cada letra
codigos :: Arbol Char -> [(Char, String)]
codigos Vacio = []
codigos (AB r izq der) 
  | r == 'R' = codigosAux izq "0" ++ codigosAux der "1"
  | otherwise = codigosAux (AB r izq der) ""

--utilizamos el auxiliar, para darle un arbol y el codigo para que nos de la frase o palabra
decodificar :: Arbol Char -> String -> String
decodificar Vacio _ = []
decodificar arbol bits = decodAux arbol bits arbol